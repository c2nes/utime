package main

//go:generate go run ./build

import (
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"strconv"
	"strings"
	"time"
	_ "time/tzdata"
	"unicode"
)

var errDSTMismatch = errors.New("DST mismatch")
var errTimeAssertionFailure = errors.New("time assertion failed")

var preferMMDD = flag.Bool("mmdd", false, "assume MM/DD for ambiguous dates")
var preferDDMM = flag.Bool("ddmm", false, "assume DD/MM for ambiguous dates")
var strictDST = flag.Bool("strict-dst", false, "enforce correct use of DST timezone abbreviations")
var debug = flag.Bool("debug", false, "debug")
var layout = flag.String("format", time.RFC1123, "output format for local and target")
var jsonOutput = flag.Bool("json", false, "output in JSON")

const Year3000Epoch = 32503680000

func abs(x int64) int64 {
	if x < 0 {
		return -x
	}
	return x
}

type Month int

const (
	Jan Month = iota + 1
	Feb
	Mar
	Apr
	May
	Jun
	Jul
	Aug
	Sep
	Oct
	Nov
	Dec
)

func (m Month) String() string {
	return []string{
		"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec",
	}[int(m)-1]
}

type DayOfWeek int

const (
	Sun DayOfWeek = iota
	Mon
	Tue
	Wed
	Thu
	Fri
	Sat
)

type Year int
type Day int

type LocalTime struct {
	hour int
	min  int
	sec  int
	nsec int
}

type MaybeLocalTimeAssertion struct {
	t  LocalTime
	tz any
}

type TimeOfDay int

const (
	AM TimeOfDay = iota
	PM
	TwentyFourHour
)

type Keyword int

const (
	At Keyword = iota
	In
	Now
	Today
	OrdinalSuffix
)

type DeltaSuffix int

const (
	After  DeltaSuffix = 1
	Before DeltaSuffix = -1
)

func (d DeltaSuffix) ToDirection() Direction {
	if d == After {
		return Forwards
	} else {
		return Backwards
	}
}

type DeltaPrefix int

const (
	Plus  DeltaPrefix = 1
	Minus DeltaPrefix = -1
)

func (d DeltaPrefix) ToDirection() Direction {
	if d == Plus {
		return Forwards
	} else {
		return Backwards
	}
}

func (d DeltaPrefix) String() string {
	if int(d) > 0 {
		return "+"
	}
	return "-"
}

type Unit int

const (
	None Unit = iota
	Mixed
	UnitNanosecond
	UnitMicrosecond
	UnitMillisecond
	UnitSecond
	UnitMinute
	UnitHour
	UnitDay
	UnitWeek
	UnitMonth
	UnitYear
)

func (u Unit) String() string {
	return []string{
		"none", "mixed", "ns", "us", "ms", "sec", "min",
		"hour", "day", "week", "month", "year",
	}[int(u)]
}

func (u Unit) FormatDuration(start, end time.Time) string {
	d := end.Sub(start)
	switch u {
	case Mixed:
		sign := ""
		if start.After(end) {
			start, end = end, start
			sign = "-"
		}
		y, m, d := 0, 0, 0
		for start.AddDate(y+1, m, d).Before(end) {
			y++
		}
		for start.AddDate(y, m+1, d).Before(end) {
			m++
		}
		for start.AddDate(y, m, d+1).Before(end) {
			d++
		}
		rem := end.Sub(start.AddDate(y, m, d))
		hr := int(rem.Hours())
		rem -= time.Duration(hr) * time.Hour
		min := int(rem.Minutes())
		rem -= time.Duration(min) * time.Minute
		sec := rem.Seconds()
		parts := []struct {
			amt  int
			unit string
		}{
			{y, "year"},
			{m, "month"},
			{d, "day"},
			{hr, "hour"},
			{min, "minute"},
		}
		var out []string
		for _, part := range parts {
			if part.amt != 0 {
				if part.amt > 1 {
					out = append(out, fmt.Sprintf("%d %ss", part.amt, part.unit))
				} else {
					out = append(out, fmt.Sprintf("%d %s", part.amt, part.unit))
				}
			}
		}
		if sec > 0 || len(out) == 0 {
			if sec == 1.0 {
				out = append(out, "1 second")
			} else {
				out = append(out, fmt.Sprintf("%.9g seconds", sec))
			}
		}
		return sign + strings.Join(out, " ")
	case UnitNanosecond:
		return fmt.Sprintf("%d ns", d.Nanoseconds())
	case UnitMicrosecond:
		return fmt.Sprintf("%d µs", d.Microseconds())
	case UnitMillisecond:
		return fmt.Sprintf("%d ms", d.Milliseconds())
	case UnitSecond:
		return fmt.Sprintf("%.1f sec", d.Seconds())
	case UnitMinute:
		return fmt.Sprintf("%.1f min", d.Minutes())
	case UnitHour:
		return fmt.Sprintf("%.1f hours", d.Hours())
	case UnitDay:
		return fmt.Sprintf("%.1f days", d.Hours()/24)
	case UnitWeek:
		return fmt.Sprintf("%.1f weeks", d.Hours()/24/7)
	case UnitMonth:
		return fmt.Sprintf("%.1f months", d.Hours()/24/365*12)
	case UnitYear:
		return fmt.Sprintf("%.1f years", d.Hours()/24/365)
	}
	panic("unsupported unit")
}

type DurationMeasure int

const (
	Since DurationMeasure = -1
	Until DurationMeasure = 1
)

type QuantityOrOffset string

type ImpliedLocation *time.Location

type ZoneOffset struct {
	Hours   int64
	Minutes int64
	Seconds int64
}

type OutputMeasure struct {
	Unit      Unit
	Direction int
}

type Transform interface {
	Apply(time.Time) time.Time
}

type Delta struct {
	Unit      Unit
	Quantity  int64
	Frac      float64
	Direction int
}

func NewDelta(unit Unit, quantity int64, frac float64, direction int) (*Delta, error) {
	if frac != 0 && unit > UnitSecond {
		return nil, errors.New("fractional units beyond seconds not allowed")
	}
	return &Delta{
		Unit:      unit,
		Quantity:  quantity,
		Frac:      frac,
		Direction: direction,
	}, nil
}

func (d *Delta) Apply(t time.Time) time.Time {
	if d.Unit <= UnitSecond {
		w := time.Duration(d.Quantity) * time.Duration(d.Direction)
		f := d.Frac * float64(d.Direction)
		if d.Unit == UnitNanosecond {
			return t.Add(w * time.Nanosecond)
		}
		if d.Unit == UnitMicrosecond {
			t = t.Add(w * time.Microsecond)
			return t.Add(time.Duration(f*1_000) * time.Nanosecond)
		}
		if d.Unit == UnitMillisecond {
			t = t.Add(w * time.Millisecond)
			return t.Add(time.Duration(f*1_000_000) * time.Nanosecond)
		}
		t = t.Add(w * time.Second)
		return t.Add(time.Duration(f*1_000_000_000) * time.Nanosecond)
	}

	w := d.Quantity * int64(d.Direction)
	if d.Unit == UnitMinute {
		return t.Add(time.Duration(w) * time.Minute)
	}
	if d.Unit == UnitHour {
		return t.Add(time.Duration(w) * time.Hour)
	}
	if d.Unit == UnitDay {
		return t.AddDate(0, 0, int(w))
	}
	if d.Unit == UnitWeek {
		return t.AddDate(0, 0, int(7*w))
	}
	if d.Unit == UnitMonth {
		return t.AddDate(0, int(w), 0)
	}
	if d.Unit == UnitYear {
		return t.AddDate(int(w), 0, 0)
	}

	panic("unsupported unit")
}

type Truncate Unit

func (t Truncate) String() string {
	return Unit(t).String()
}

func (t Truncate) Apply(v time.Time) time.Time {
	date := func(y int, m time.Month, d int) time.Time {
		return time.Date(y, m, d, 0, 0, 0, 0, v.Location())
	}
	u := Unit(t)
	if u == UnitYear {
		return date(v.Year(), time.January, 1)
	}
	if u == UnitMonth {
		return date(v.Year(), v.Month(), 1)
	}
	if u == UnitWeek {
		return date(v.Year(), v.Month(), v.Day()).AddDate(0, 0, -int(v.Weekday()))
	}
	if u == UnitDay {
		return date(v.Year(), v.Month(), v.Day())
	}
	panic("unimplemented")
}

type Direction int

const (
	Forwards Direction = iota
	Backwards
	// Forward, but exclude "this" week/month/year
	Next
	// Backwards, but exclude "this" week/month/year
	Last
	// Forwards or backwards.
	Nearest
	// As in, "this week", "this tuesday", "this month", etc.
	This
)

func (d Direction) String() string {
	return []string{
		"forwards", "backards", "next", "last", "nearest", "this",
	}[int(d)]
}

func IsDirection(v any) bool {
	_, ok := v.(Direction)
	return ok
}

// Search for a date with a given month, day, and/or day of week.
type Search struct {
	Year      Year
	Month     Month
	Day       Day
	DayOfWeek DayOfWeek

	// Skip this many initial matches.
	Skip int64

	// Direction in which to perform the search.
	// +1, forward
	// -1, backwards
	//  0, nearest
	// -2, limit search to "this" week/month/year
	Direction Direction

	// If true, the search keyword is a suffix, indicating
	// that additional calendar components may only appear
	// before the search keyword.
	Suffix bool
}

func NewSearch(direction Direction, skip int64) *Search {
	return &Search{
		Year:      -1,
		Month:     -1,
		Day:       -1,
		DayOfWeek: -1,
		Skip:      skip,
		Direction: direction,
	}
}

func (s *Search) SetCalendar(c any) {
	if IsYear(c) {
		s.Year = c.(Year)
	} else if IsMonth(c) {
		s.Month = c.(Month)
	} else if IsDay(c) {
		s.Day = c.(Day)
	} else if IsDayOfWeek(c) {
		s.DayOfWeek = c.(DayOfWeek)
	} else {
		panic("not a calendar unit")
	}
}

func (d *Search) Apply(t time.Time) time.Time {
	week := func(tx time.Time) int {
		yr, wk := tx.ISOWeek()
		return yr*100 + wk
	}
	matchEnclosing := func(a, b int) bool {
		if d.Direction == Next || d.Direction == Last {
			return a != b
		}
		if d.Direction == This {
			return a == b
		}
		return true
	}
	match := func(tx time.Time) bool {
		// Check search components
		if d.Year >= 0 && Year(tx.Year()) != d.Year {
			return false
		}
		if d.Month >= Jan && Month(tx.Month()) != d.Month {
			return false
		}
		if d.Day >= 1 && Day(tx.Day()) != d.Day {
			return false
		}
		if d.DayOfWeek >= Sun && DayOfWeek(tx.Weekday()) != d.DayOfWeek {
			return false
		}
		// Apply rules for enclosing time period. "This" should
		// require the enclosing time period match. "Next"/"Last"
		// should require the enclosing time period increment/decrement.
		if d.Month >= Jan && !matchEnclosing(t.Year(), tx.Year()) {
			return false
		}
		if d.DayOfWeek >= Sun && !matchEnclosing(week(t), week(tx)) {
			return false
		}
		return true
	}

	var truncate Unit
	if d.Day >= 1 {
		truncate = None
	} else if d.DayOfWeek >= Sun {
		truncate = UnitDay
	} else if d.Month >= Jan {
		truncate = UnitMonth
	}

	skip := d.Skip
	var lastMatch time.Time
	for i := 0; i < 365*10000; i++ {
		var off int
		if d.Direction == Forwards || d.Direction == Next {
			off = i
		} else if d.Direction == Backwards || d.Direction == Last {
			off = -i
		} else if i%2 == 0 {
			off = i / 2
		} else {
			off = -i / 2
		}

		tx := t.AddDate(0, 0, off)
		if truncate > None {
			tx = Truncate(truncate).Apply(tx)
		}
		if match(tx) {
			if tx.Equal(lastMatch) {
				continue
			}
			if skip <= 0 {
				return tx
			}
			skip--
			lastMatch = tx
		}
	}
	return time.Time{}
}

type OutputZone struct {
	Zone any
}

func IsYear(v any) bool {
	_, ok := v.(Year)
	return ok
}

func IsMonth(v any) bool {
	_, ok := v.(Month)
	return ok
}

func IsDay(v any) bool {
	_, ok := v.(Day)
	return ok
}

func IsLocalTime(v any) bool {
	_, ok := v.(LocalTime)
	return ok
}

func IsZoneAbbrev(v any) bool {
	_, ok := v.(*ZoneAbbrev)
	return ok
}

func IsZoneOffset(v any) bool {
	_, ok := v.(ZoneOffset)
	return ok
}

func IsLocation(v any) bool {
	_, ok := v.(*time.Location)
	return ok
}

func IsTimezone(v any) bool {
	return IsZoneAbbrev(v) || IsZoneOffset(v) || IsLocation(v)
}

func IsTimeOfDay(v any) bool {
	_, ok := v.(TimeOfDay)
	return ok
}

func IsDayOfWeek(v any) bool {
	_, ok := v.(DayOfWeek)
	return ok
}

func IsDayOrMonth(v any) bool {
	return IsDay(v) || IsMonth(v)
}

func IsDateComponent(v any) bool {
	return IsYear(v) || IsMonth(v) || IsDay(v) || IsTimezone(v)
}

func IsTimeComponent(v any) bool {
	return IsLocalTime(v) || IsTimeOfDay(v)
}

func IsDateTimeComponent(v any) bool {
	return IsDateComponent(v) || IsTimeComponent(v)
}

func IsUnit(v any) bool {
	_, ok := v.(Unit)
	return ok
}

func IsDeltaPrefix(v any) bool {
	_, ok := v.(DeltaPrefix)
	return ok
}

func IsDeltaSuffix(v any) bool {
	_, ok := v.(DeltaSuffix)
	return ok
}

func IsInt64(v any) bool {
	_, ok := v.(int64)
	return ok
}

func IsFloat64(v any) bool {
	_, ok := v.(float64)
	return ok
}

func IsQuantity(v any) bool {
	return IsInt64(v) || IsFloat64(v)
}

func CouldBeYear(v any) bool {
	n, ok := v.(int64)
	return ok && 1900 <= n && n < 3000
}

func IsQuantityOrOffset(v any) bool {
	_, ok := v.(QuantityOrOffset)
	return ok
}

func IsDurationMeasure(v any) bool {
	_, ok := v.(DurationMeasure)
	return ok
}

func IsOutputMeasure(v any) bool {
	_, ok := v.(OutputMeasure)
	return ok
}

func IsSearch(v any) bool {
	_, ok := v.(*Search)
	return ok
}

func IsSuffixedSearch(v any) bool {
	s, ok := v.(*Search)
	return ok && s.Suffix
}

func IsPrefixedSearch(v any) bool {
	s, ok := v.(*Search)
	return ok && !s.Suffix
}

func Is(want any) func(v any) bool {
	return func(v any) bool {
		return v == want
	}
}

func Not(p func(any) bool) func(any) bool {
	return func(v any) bool {
		return !p(v)
	}
}

func Or(p ...func(any) bool) func(any) bool {
	return func(a any) bool {
		for _, px := range p {
			if px(a) {
				return true
			}
		}
		return false
	}
}

type ResolvedTimezone struct {
	loc         *time.Location
	dstExpected bool
	dstEnforce  bool
}

func ResolveTimezone(v any) (*ResolvedTimezone, error) {
	if v, ok := v.(ZoneOffset); ok {
		loc := time.FixedZone("", int(v.Hours*3600+v.Minutes*60+v.Seconds))
		return &ResolvedTimezone{loc, false, false}, nil
	}
	if v, ok := v.(*ZoneAbbrev); ok {
		loc, err := time.LoadLocation(v.Zone)
		if err != nil {
			return nil, err
		}
		dstExpected := v.IsDST
		dstEnforce := true
		return &ResolvedTimezone{loc, dstExpected, dstEnforce}, nil
	}
	if loc, ok := v.(*time.Location); ok {
		return &ResolvedTimezone{loc, false, false}, nil
	}
	return nil, errors.New("not a timezone")
}

func DebugUnits(units []any) string {
	var out []string
	for _, unit := range units {
		if l, ok := unit.(ImpliedLocation); ok {
			out = append(out, fmt.Sprintf("%T(%s)", unit, (*time.Location)(l)))
		} else if s, ok := unit.(fmt.Stringer); ok {
			out = append(out, fmt.Sprintf("%T(%s)", unit, s))
		} else {
			out = append(out, fmt.Sprintf("%T(%#v)", unit, unit))
		}
	}
	return strings.Join(out, ",")
}

// AM, PM
// january, february, ...
// sunday, monday, ...
// 2024
// 16
// 16:24
// 12:12pm
// 12:12 pm
// 2023-04-01
// 2023-04-01T01:02:03.000
// Zone abbreviation
// YYYYMMDD
// HHMM
// noon
// midnight

// Relative times
// --------------------
// now, yesterday, tomorrow
// last year|week|month|jan|feb|sunday|monday|...
// next ..
// "ago"

// Modifiers
// "at" <time>

// Durations
// --------------
// 1 day, week, month, year, hour, minute, second
// 2 days, weeks, months, years, hours, minutes, seconds

// Offsets
// --------------
// before, after
// +
// -

/* // Relative time terms
Now
Yesterday
Tomorrow
*/

type Rule struct {
	Pattern []func(v any) bool
	Merge   func(xs []any) ([]any, error)
}

func Match(pattern ...func(v any) bool) []func(v any) bool {
	return pattern
}

func NewRule(pattern []func(v any) bool, merge func(xs []any) ([]any, error)) *Rule {
	return &Rule{pattern, merge}
}

func (r *Rule) Len() int {
	return len(r.Pattern)
}

func (r *Rule) Matches(xs []any) bool {
	if len(xs) < r.Len() {
		return false
	}
	for i, p := range r.Pattern {
		if !p(xs[i]) {
			return false
		}
	}
	return true
}

func (r *Rule) Apply(xs []any, idx int) ([]any, bool, error) {
	if r.Matches(xs[idx:]) {
		pre := xs[:idx]
		match := xs[idx : idx+r.Len()]
		post := xs[idx+r.Len():]
		var err error
		match, err = r.Merge(match)
		if err != nil {
			return nil, false, err
		}

		newLen := len(pre) + len(match) + len(post)
		if newLen < cap(xs) {
			xs = xs[:newLen]
		} else {
			xs = make([]any, newLen)
		}
		// We must copy rest first. If we copy merged first and len(merged) > len(head)
		// then we will overwrite elements of `rest` if we are reusing the backing array.
		copy(xs[len(pre)+len(match):], post)
		copy(xs[len(pre):], match)
		copy(xs, pre)
		return xs, true, nil
	}
	return xs, false, nil
}

func applyRules(xs []any, rules ...*Rule) ([]any, error) {
beginning:
	for {
		for _, rule := range rules {
			for i := 0; i < len(xs); i++ {
				var matched bool
				var err error
				xs, matched, err = rule.Apply(xs, i)
				if err != nil {
					return nil, err
				}
				if matched {
					if *debug {
						log.Println("->", DebugUnits(xs))
					}
					continue beginning
				}
			}
		}
		break
	}
	return xs, nil
}

func tokenize(s string) []string {
	type class int
	const (
		Digit class = iota
		Letter
		Separator
		Space
		Other
	)

	const separators = ":-./+'_"
	getClass := func(c rune) class {
		if unicode.IsDigit(c) {
			return Digit
		}
		if unicode.IsLetter(c) {
			return Letter
		}
		if strings.ContainsRune(separators, c) {
			return Separator
		}
		if unicode.IsSpace(c) {
			return Space
		}
		return Other
	}

	merge := func(a, b, x class) bool {
		if x == Space || b == Space {
			return false
		}
		if x == Letter {
			return b == Letter || (b == Separator && (a == Letter || a == Space))
		}
		if x == Digit {
			return b == Digit || (b == Separator && (a == Digit || a == Space))
		}
		if x == Separator {
			return true
		}
		return false
	}

	var tokens []string
	var buf strings.Builder

	flush := func() {
		s := buf.String()
		trimmed := strings.TrimRight(s, separators)
		if trimmed != "" {
			s = trimmed
		}

		// Hueristic to split date from time. Very ugly. Find a unique separator before
		// a time component that best splits the string in half.
		if len(s) > 2 {
			counts := make(map[rune]int)
			var idxs []int
			var totalCount int
			for i, c := range s[1 : len(s)-1] {
				// Do not split at or after ":"
				if c == ':' {
					break
				}
				if strings.ContainsRune(separators, c) {
					counts[c] += 1
					totalCount += 1
					idxs = append(idxs, i+1)
				}
			}
			if totalCount > 2 {
				bestIdx := -1
				bestIdxErr := float64(len(s))
				for _, idx := range idxs {
					if counts[rune(s[idx])] == 1 {
						idxErr := math.Abs(float64(len(s))/2 - float64(idx))
						if idxErr < bestIdxErr {
							bestIdx = idx
							bestIdxErr = idxErr
						}
					}
				}
				if bestIdx >= 0 {
					tokens = append(tokens, s[:bestIdx], s[bestIdx+1:])
					s = ""
				}
			}
		}

		if len(s) > 0 {
			tokens = append(tokens, s)
		}
		buf.Reset()
	}

	a := Space
	b := Space
	for _, c := range s {
		x := getClass(c)
		if !merge(a, b, x) {
			flush()
		}
		if x != Space && x != Other {
			buf.WriteRune(c)
		}
		a = b
		b = x
	}
	flush()
	return tokens
}

func oneof(patterns ...string) *regexp.Regexp {
	return regexp.MustCompile("^(?:" + strings.Join(patterns, "|") + ")$")
}

func parseInt(s string) int64 {
	i, err := strconv.ParseInt(s, 10, 64)
	if err != nil {
		panic(err)
	}
	return i
}

func parseFloat(s string) float64 {
	f, err := strconv.ParseFloat(s, 64)
	if err != nil {
		panic(err)
	}
	return f
}

type Result struct {
	Time time.Time

	DeltaBase      time.Time
	DeltaUnit      Unit
	DeltaDirection int
}

func parse(now time.Time, s string) (*Result, error) {
	tokens := tokenize(strings.ToLower(s))

	var units []any

	explodeTimestamp := func(t time.Time) []any {
		return []any{
			Year(t.Year()),
			Month(t.Month()),
			Day(t.Day()),
			LocalTime{
				hour: t.Hour(),
				min:  t.Minute(),
				sec:  t.Second(),
				nsec: t.Nanosecond(),
			},
			TwentyFourHour,
			ImpliedLocation(t.Location()),
		}
	}

	explodeDate := func(t time.Time) []any {
		return []any{
			Year(t.Year()),
			Month(t.Month()),
			Day(t.Day()),
			ImpliedLocation(t.Location()),
		}
	}

	regexpQuantityOrOffset := oneof(`[+-](?:\d\d){1,3}`)
	regexpSignedQuantity := oneof(`[+-]\d+`)
	regexpQuantity := oneof(`\d+`)
	regexpTime := oneof(`(\d+):(\d\d)(?::(\d\d(?:\.\d+)?))?([+-]\d\d:?\d\d)?`)
	regexpDate := oneof(
		`(?P<year>\d{4})-(?P<month>\d{2})-(?<day>\d{2})`,
		`(?P<year>\d{4})/(?P<month>\d{1,2})/(?<day>\d{1,2})`,
		`(?P<day>\d{2})\.(?P<month>\d{2})\.(?<year>\d{4})`,
		`(?P<daymonth>\d{1,2})/(?P<daymonth>\d{1,2})/(?<year>\d{4})`,
		`(?P<daymonth>[0-3]\d)(?P<daymonth>[0-3]\d)(?P<year>(?:19|20)\d{2})`,
		`(?P<year>(?:19|20)\d{2})(?P<month>[0-1]\d)(?<day>[0-3]\d)`,
		`(?P<daymonth>\d{2})/(?P<daymonth>\d{2})`,
	)
	regexpShortYear := regexp.MustCompile(`^'(\d\d)$`)

	skip := map[string]bool{
		"on": true,
		"t":  true,
	}

	literals := map[string]any{
		// AM/PM
		"am": AM,
		"pm": PM,
		// Short months
		"jan": Jan,
		"feb": Feb,
		"mar": Mar,
		"apr": Apr,
		"may": May,
		"jun": Jun,
		"jul": Jul,
		"aug": Aug,
		"sep": Sep,
		"oct": Oct,
		"nov": Nov,
		"dec": Dec,
		// Long months
		"january":   Jan,
		"february":  Feb,
		"march":     Mar,
		"april":     Apr,
		"june":      Jun,
		"july":      Jul,
		"august":    Aug,
		"september": Sep,
		"october":   Oct,
		"november":  Nov,
		"december":  Dec,
		// Short day of week
		"sun": Sun,
		"mon": Mon,
		"tue": Tue,
		"wed": Wed,
		"thu": Thu,
		"fri": Fri,
		"sat": Sat,
		// Full day of week
		"sunday":    Sun,
		"monday":    Mon,
		"tuesday":   Tue,
		"wednesday": Wed,
		"thursday":  Thu,
		"friday":    Fri,
		"saturday":  Sat,
		// Modifiers
		"last":   Last,
		"next":   Next,
		"plus":   Plus,
		"+":      Plus,
		"minus":  Minus,
		"-":      Minus,
		"this":   This,
		"after":  After,
		"from":   After,
		"before": Before,
		"at":     At,
		"in":     In,
		"now":    Now,
		"today":  Today,
		// Ordinal suffixes
		"st": OrdinalSuffix,
		"nd": OrdinalSuffix,
		"rd": OrdinalSuffix,
		"th": OrdinalSuffix,
		// Units
		"nanosecond":   UnitNanosecond,
		"nanoseconds":  UnitNanosecond,
		"microsecond":  UnitMicrosecond,
		"microseconds": UnitMicrosecond,
		"millisecond":  UnitMillisecond,
		"milliseconds": UnitMillisecond,
		"second":       UnitSecond,
		"seconds":      UnitSecond,
		"minute":       UnitMinute,
		"minutes":      UnitMinute,
		"hour":         UnitHour,
		"hours":        UnitHour,
		"day":          UnitDay,
		"days":         UnitDay,
		"week":         UnitWeek,
		"weeks":        UnitWeek,
		"month":        UnitMonth,
		"months":       UnitMonth,
		"year":         UnitYear,
		"years":        UnitYear,
		// Unit abbreviations
		"ns":  UnitNanosecond,
		"us":  UnitMicrosecond,
		"ms":  UnitMillisecond,
		"s":   UnitSecond,
		"m":   UnitMinute,
		"min": UnitMinute,
		"h":   UnitHour,
		"hr":  UnitHour,
		"d":   UnitDay,
		"w":   UnitWeek,
		"mn":  UnitMonth,
		"y":   UnitYear,
		"yr":  UnitYear,
		// Duration measurements
		"since": Since,
		"until": Until,
	}

	if *debug {
		log.Printf("tokens: %#v", tokens)
	}

	for _, token := range tokens {
		if match := regexpTime.FindStringSubmatch(token); match != nil {
			lt := LocalTime{
				hour: int(parseInt(match[1])),
				min:  int(parseInt(match[2])),
			}
			if match[3] != "" {
				fsec := match[3]
				nidx := strings.IndexRune(fsec, '.')
				if nidx < 0 {
					lt.sec = int(parseInt(fsec))
				} else {
					lt.sec = int(parseInt(fsec[:nidx]))
					lt.nsec = int(math.Round(1e9 * parseFloat(fsec[nidx:])))
				}
			}
			units = append(units, lt)
			if match[4] != "" {
				off := match[4]
				hh := parseInt(off[:3])
				mm := parseInt(off[len(off)-2:])
				units = append(units, ZoneOffset{hh, mm, 0})
			}
		} else if token == "noon" {
			units = append(units, LocalTime{hour: 12}, TwentyFourHour)
		} else if token == "midnight" {
			units = append(units, LocalTime{hour: 0}, TwentyFourHour)
		} else if match := regexpDate.FindStringSubmatch(token); match != nil {
			m := make(map[string]string)
			for i, k := range regexpDate.SubexpNames() {
				if match[i] != "" {
					m[k] = match[i]
				}
			}
			if v := m["year"]; v != "" {
				units = append(units, Year(parseInt(v)))
			}
			if v := m["month"]; v != "" {
				units = append(units, Month(parseInt(v)))
			}
			if v := m["day"]; v != "" {
				units = append(units, Day(parseInt(v)))
			}

			var daymonths []int64
			for i, k := range regexpDate.SubexpNames() {
				if k == "daymonth" && match[i] != "" {
					daymonths = append(daymonths, parseInt(match[i]))
				}
			}

			if len(daymonths) == 2 {
				if daymonths[0] > 12 {
					units = append(units, Day(daymonths[0]), Month(daymonths[1]))
				} else if daymonths[1] > 12 {
					units = append(units, Month(daymonths[0]), Day(daymonths[1]))
				} else if *preferMMDD {
					units = append(units, Month(daymonths[0]), Day(daymonths[1]))
				} else if *preferDDMM {
					units = append(units, Day(daymonths[0]), Month(daymonths[1]))
				} else {
					return nil, errors.New("ambiguous day/month")
				}
			} else if len(daymonths) != 0 {
				panic("exactly 0 or 2 daymonths needed")
			}
		} else if match := regexpShortYear.FindStringSubmatch(token); match != nil {
			yr := parseInt(match[1])
			// 1969 cutoff is consistent with the time pkg
			if yr < 69 {
				yr += 2000
			} else {
				yr += 1900
			}
			units = append(units, Year(yr))
		} else if unit := literals[token]; unit != nil {
			units = append(units, unit)
		} else if token == "utc" {
			units = append(units, time.UTC)
		} else if zone := ZoneAbbrevs[strings.ToUpper(token)]; zone != nil {
			units = append(units, zone)
		} else if zone := Zones[token]; zone != "" {
			loc, err := time.LoadLocation(zone)
			if err != nil {
				return nil, err
			}
			units = append(units, loc)
		} else if token == "ago" {
			// "<blank> ago" is the same as "<blank> before now"
			units = append(units, Before, Now)
		} else if token == "yesterday" {
			units = append(units, int64(1), UnitDay, Before, Today)
		} else if token == "tomorrow" {
			units = append(units, int64(1), UnitDay, After, Today)
		} else if regexpQuantityOrOffset.MatchString(token) {
			units = append(units, QuantityOrOffset(token))
		} else if regexpSignedQuantity.MatchString(token) {
			var d DeltaPrefix
			if token[0] == '-' {
				d = Minus
			} else {
				d = Plus
			}
			n := parseInt(token[1:])
			units = append(units, d, n)
		} else if regexpQuantity.MatchString(token) {
			units = append(units, parseInt(token))
		} else if n, err := strconv.ParseFloat(token, 64); err == nil {
			units = append(units, n)
		} else if !skip[token] {
			return nil, fmt.Errorf("can not parse %q", token)
		}
	}

	rules := []*Rule{
		// Delta with prefix
		NewRule(
			Match(IsDeltaPrefix, IsQuantity, IsUnit),
			func(xs []any) ([]any, error) {
				d := xs[0].(DeltaPrefix)
				q := xs[1]
				u := xs[2].(Unit)
				var w int64
				var f float64
				if n, ok := q.(int64); ok {
					w = n
				}
				if n, ok := q.(float64); ok {
					w = int64(n)
					f = n - float64(w)
				}
				delta, err := NewDelta(u, w, f, int(d))
				if err != nil {
					return nil, err
				}
				return []any{delta}, nil
			},
		),
		// Delta with suffix
		NewRule(
			Match(IsQuantity, IsUnit, IsDeltaSuffix),
			func(xs []any) ([]any, error) {
				q := xs[0]
				u := xs[1].(Unit)
				d := xs[2].(DeltaSuffix)
				var w int64
				var f float64
				if n, ok := q.(int64); ok {
					w = n
				}
				if n, ok := q.(float64); ok {
					w = int64(n)
					f = n - float64(w)
				}
				return []any{
					&Delta{
						Unit:      u,
						Quantity:  w,
						Frac:      f,
						Direction: int(d),
					},
				}, nil
			},
		),
		// Quantities preceeding a time of day indicator are hours
		NewRule(
			Match(IsInt64, IsTimeOfDay),
			func(xs []any) ([]any, error) {
				return []any{
					LocalTime{
						hour: int(xs[0].(int64)),
					},
					xs[1],
				}, nil
			},
		),
		NewRule(
			Match(IsDeltaPrefix, IsInt64, Or(IsMonth, IsDay, IsDayOfWeek)),
			func(xs []any) ([]any, error) {
				d := xs[0].(DeltaPrefix).ToDirection()
				s := NewSearch(d, xs[1].(int64)-1)
				s.SetCalendar(xs[2])
				return []any{s}, nil
			},
		),
		NewRule(
			Match(Or(IsMonth, IsDay, IsDayOfWeek), IsDeltaSuffix),
			func(xs []any) ([]any, error) {
				d := xs[1].(DeltaSuffix).ToDirection()
				s := NewSearch(d, 0)
				s.SetCalendar(xs[0])
				return []any{s}, nil
			},
		),
		NewRule(
			Match(IsPrefixedSearch, Or(IsMonth, IsInt64, IsDay, IsDayOfWeek)),
			func(xs []any) ([]any, error) {
				s := xs[0].(*Search)
				if v, ok := xs[1].(int64); ok {
					xs[1] = Day(v)
				}
				s.SetCalendar(xs[1])
				return []any{s}, nil
			},
		),
		NewRule(
			Match(Or(IsMonth, IsDay, IsDayOfWeek), IsSuffixedSearch),
			func(xs []any) ([]any, error) {
				s := xs[1].(*Search)
				s.SetCalendar(xs[0])
				return []any{s}, nil
			},
		),
		NewRule(
			Match(IsDirection, Or(IsMonth, IsDay, IsDayOfWeek)),
			func(xs []any) ([]any, error) {
				s := NewSearch(xs[0].(Direction), 0)
				s.SetCalendar(xs[1])
				return []any{s}, nil
			},
		),
		NewRule(
			Match(IsDirection, IsUnit),
			func(xs []any) ([]any, error) {
				d := xs[0].(Direction)
				u := xs[1].(Unit)
				if d == This {
					return []any{Truncate(u)}, nil
				}
				delta := &Delta{
					Unit:     u,
					Quantity: 1,
				}
				if d == Next {
					delta.Direction = 1
				}
				if d == Last {
					delta.Direction = -1
				}
				return []any{delta, Truncate(u)}, nil
			},
		),
		// Apply time of day indicator
		NewRule(
			Match(IsLocalTime, IsTimeOfDay),
			func(xs []any) ([]any, error) {
				lt := xs[0].(LocalTime)
				tod := xs[1].(TimeOfDay)
				if lt.hour > 12 && tod != TwentyFourHour {
					return nil, errors.New("AM/PM invalid with hour greater than 12")
				}
				if lt.hour < 12 && tod == PM {
					lt.hour += 12
				} else if lt.hour == 12 && tod == AM {
					lt.hour = 0
				}
				return []any{lt}, nil
			},
		),
		// at <time> might be either setting or asserting the time
		NewRule(
			Match(Is(At), IsLocalTime, IsTimezone),
			func(xs []any) ([]any, error) {
				lt := xs[1].(LocalTime)
				tz := xs[2]
				return []any{
					MaybeLocalTimeAssertion{lt, tz},
				}, nil
			},
		),
		NewRule(
			Match(Is(At), IsLocalTime),
			func(xs []any) ([]any, error) {
				lt := xs[1].(LocalTime)
				return []any{
					MaybeLocalTimeAssertion{lt, nil},
				}, nil
			},
		),
		// Numbers before ordinal suffixes (e.g. 1st, 2nd, etc.) are days
		NewRule(
			Match(IsInt64, Is(OrdinalSuffix)),
			func(xs []any) ([]any, error) {
				day := xs[0].(int64)
				return []any{Day(day)}, nil
			},
		),
		// Infer days and years based on proximity to days/months
		NewRule(
			Match(IsDayOrMonth, IsInt64),
			func(xs []any) ([]any, error) {
				q := xs[1].(int64)
				var unit any
				if q <= 31 {
					unit = Day(q)
				} else {
					unit = Year(q)
				}
				return []any{xs[0], unit}, nil
			},
		),
		NewRule(
			Match(IsInt64, IsDayOrMonth),
			func(xs []any) ([]any, error) {
				q := xs[0].(int64)
				var unit any
				if q <= 31 {
					unit = Day(q)
				} else {
					unit = Year(q)
				}
				return []any{unit, xs[1]}, nil
			},
		),
		// Correct "day month day" to "day month year" for short years
		NewRule(
			Match(IsDay, IsMonth, IsDay),
			func(xs []any) ([]any, error) {
				d := xs[0]
				m := xs[1]
				y := int(xs[2].(Day))
				return []any{d, m, Year(2000 + y)}, nil
			},
		),
		// Disambiguate zone offsets from quantities (e.g. +0500)
		NewRule(
			Match(IsQuantityOrOffset, IsUnit),
			func(xs []any) ([]any, error) {
				s := string(xs[0].(QuantityOrOffset))
				var d DeltaPrefix
				if s[0] == '-' {
					d = Minus
				} else {
					d = Plus
				}
				q := parseInt(s[1:])
				return []any{d, q, xs[1]}, nil
			},
		),
		// QuantityOrOffset following a date/time component
		// will assumed to be offsets if they do not preceed
		// a unit.
		NewRule(
			Match(IsDateTimeComponent, IsQuantityOrOffset),
			func(xs []any) ([]any, error) {
				off := string(xs[1].(QuantityOrOffset))
				h := parseInt(off[:3])
				var m int64
				if len(off) > 3 {
					m = parseInt(off[3:5])
				}
				var s int64
				if len(off) > 5 {
					s = parseInt(off[5:6])
				}
				if h < 0 {
					m *= -1
					s *= -1
				}
				return []any{xs[0], ZoneOffset{h, m, s}}, nil
			},
		),
		NewRule(
			Match(IsQuantityOrOffset),
			func(xs []any) ([]any, error) {
				s := string(xs[0].(QuantityOrOffset))
				var d DeltaPrefix
				if s[0] == '-' {
					d = Minus
				} else {
					d = Plus
				}
				q := parseInt(s[1:])
				return []any{d, q}, nil
			},
		),
		// Remaining quantity that is plausibly a year
		NewRule(
			Match(CouldBeYear),
			func(xs []any) ([]any, error) {
				q := xs[0].(int64)
				return []any{Year(q)}, nil
			},
		),
		// Remaining quantities are treated as unix timestamps
		NewRule(
			Match(IsInt64),
			func(xs []any) ([]any, error) {
				sec := xs[0].(int64)
				scale := int64(1)
				for abs(sec/scale) > Year3000Epoch {
					scale *= 1000
				}
				nsec := (1000000000 / scale) * (sec % scale)
				sec = sec / scale
				return explodeTimestamp(time.Unix(sec, nsec).UTC()), nil
			},
		),
		NewRule(
			Match(IsFloat64),
			func(xs []any) ([]any, error) {
				fsec := xs[0].(float64)
				sec := int64(fsec)
				nsec := int64((fsec - float64(sec)) * 1e9)
				return explodeTimestamp(time.Unix(sec, nsec).UTC()), nil
			},
		),
		// Output timezone
		NewRule(
			Match(Is(In), IsTimezone),
			func(xs []any) ([]any, error) {
				return []any{OutputZone{xs[1]}}, nil
			},
		),
		NewRule(
			Match(IsUnit, IsDurationMeasure),
			func(xs []any) ([]any, error) {
				unit := xs[0].(Unit)
				direction := (int)(xs[1].(DurationMeasure))
				return []any{OutputMeasure{unit, direction}}, nil
			},
		),
		NewRule(
			Match(IsDurationMeasure),
			func(xs []any) ([]any, error) {
				direction := (int)(xs[0].(DurationMeasure))
				return []any{OutputMeasure{Mixed, direction}}, nil
			},
		),
	}

	if *debug {
		log.Println(DebugUnits(units))
	}

	var err error
	units, err = applyRules(units, rules...)
	if err != nil {
		return nil, err
	}

	// Resolve the source timezone before we expand "now" and "today" references
	sourceTimezoneIsExplicit := false
	sourceTimezone := &ResolvedTimezone{
		loc:         now.Location(),
		dstExpected: false,
		dstEnforce:  false,
	}

	for _, unit := range units {
		if IsTimezone(unit) {
			var err error
			sourceTimezone, err = ResolveTimezone(unit)
			if err != nil {
				return nil, nil
			}
			sourceTimezoneIsExplicit = true
		}
		if zone, ok := unit.(ImpliedLocation); ok && !sourceTimezoneIsExplicit {
			var err error
			sourceTimezone, err = ResolveTimezone((*time.Location)(zone))
			if err != nil {
				return nil, nil
			}
		}
	}

	// Expand "today" and "now" references in the source timezone
	now = now.In(sourceTimezone.loc)
	units, _ = applyRules(units,
		NewRule(Match(Is(Today)), func(xs []any) ([]any, error) {
			return explodeDate(now), nil
		}),
		NewRule(Match(Is(Now)), func(xs []any) ([]any, error) {
			return explodeTimestamp(now), nil
		}))

	// Apply rules once more to process expanded today/now references.
	units, err = applyRules(units, rules...)
	if err != nil {
		return nil, err
	}

	// "at" resets the time component. Remove all time components before the last "at".
	merged := make([]any, 0, len(units))
	for _, unit := range units {
		// Reset time of day when the "at" keyword appears
		if unit == At {
			var filtered []any
			for _, v := range merged {
				if !IsTimeComponent(v) {
					filtered = append(filtered, v)
				}
			}
			merged = filtered
		}

		merged = append(merged, unit)
	}
	units = merged

	// Debug logging
	if *debug {
		log.Println("->", DebugUnits(units))
	}

	// Check for bare quantities and other errors
	for _, unit := range units {
		if _, ok := unit.(Unit); ok {
			return nil, errors.New("bare unit")
		}
		if _, ok := unit.(DeltaPrefix); ok {
			return nil, errors.New("unexpected +/-")
		}
		if _, ok := unit.(DeltaSuffix); ok {
			return nil, errors.New("unexpected before/after")
		}
	}

	// Check for redundant specifiers
	count := func(xs []any, f func(v any) bool) int {
		n := 0
		for _, x := range xs {
			if f(x) {
				n++
			}
		}
		return n
	}
	if count(units, IsLocalTime) > 1 {
		return nil, errors.New("multiple times specified")
	}
	if count(units, IsDay) > 1 {
		return nil, errors.New("multiple days specified")
	}
	if count(units, IsMonth) > 1 {
		return nil, errors.New("multiple months specified")
	}
	if count(units, IsYear) > 1 {
		return nil, errors.New("multiple years specified")
	}
	if count(units, IsTimeOfDay) > 0 {
		return nil, errors.New("multiple times of day specified")
	}
	if count(units, IsDayOfWeek) > 1 {
		return nil, errors.New("multiple days of the week specified")
	}
	if count(units, IsTimezone) > 1 {
		return nil, errors.New("multiple timezones specified")
	}
	if count(units, IsOutputMeasure) > 1 {
		return nil, errors.New("multiple uses of until/since")
	}

	year := now.Year()
	yearIsSet := false
	month := now.Month()
	monthIsSet := false
	day := now.Day()
	dayIsSet := false
	hour := now.Hour()
	min := now.Minute()
	sec := int64(now.Second())
	nsec := int64(now.Nanosecond())
	timeIsSet := false
	dayOfWeek := now.Weekday()
	dayOfWeekIsSet := false

	for _, unit := range units {
		if v, ok := unit.(Year); ok {
			year = int(v)
			yearIsSet = true
		}
		if v, ok := unit.(Month); ok {
			month = time.Month(v)
			monthIsSet = true
		}
		if v, ok := unit.(Day); ok {
			day = int(v)
			dayIsSet = true
		}
		if v, ok := unit.(LocalTime); ok {
			hour = v.hour
			min = v.min
			sec = int64(v.sec)
			nsec = int64(v.nsec)
			timeIsSet = true
		}
		if v, ok := unit.(DayOfWeek); ok {
			dayOfWeek = time.Weekday(v)
			dayOfWeekIsSet = true
		}
	}

	if yearIsSet && (!monthIsSet && !dayIsSet) {
		// Year only. Set to Jan 1.
		month = time.January
		monthIsSet = true
		day = 1
		dayIsSet = true
	} else if monthIsSet && !dayIsSet {
		// Month, but no day. Set to the 1st.
		day = 1
		dayIsSet = true
	}

	// If any date component is specified, but no time is given
	// then default time to midnight.
	if !timeIsSet && (yearIsSet || monthIsSet || dayIsSet) {
		hour = 0
		min = 0
		sec = 0
		nsec = 0
	}

	// If the day, month, or year is not set then we'll search forwards
	// and backwards from `now` to find the closest match. Measurements
	// (i.e. "since" / "from") will change this behavior so we instead
	// look backwards or forwards only.
	if !dayIsSet || !monthIsSet || !yearIsSet {
		search := NewSearch(Nearest, 0)
		if yearIsSet {
			search.SetCalendar(Year(year))
		}
		if monthIsSet {
			search.SetCalendar(Month(month))
		}
		if dayIsSet {
			search.SetCalendar(Day(day))
		}
		if dayOfWeekIsSet {
			search.SetCalendar(DayOfWeek(dayOfWeek))
		}
		for _, unit := range units {
			// days until <...> should always look forward
			// days since <...> should always look backwards
			if m, ok := unit.(OutputMeasure); ok {
				if m.Direction > 0 {
					search.Direction = Forwards
				}
				if m.Direction < 0 {
					search.Direction = Backwards
				}
			}
		}
		if match := search.Apply(now); !match.IsZero() {
			if !yearIsSet {
				year = match.Year()
			}
			if !monthIsSet {
				month = match.Month()
			}
			if !dayIsSet {
				day = match.Day()
			}
			if !dayOfWeekIsSet {
				dayOfWeek = match.Weekday()
			}
		}
	}

	// TODO: Handle ambiguous times at DST boundaries when an hour occurs twice
	result := time.Date(year, month, day, hour, min, int(sec), int(nsec), sourceTimezone.loc)

	// Verify that any asserted calendar components are as expected
	if (dayOfWeekIsSet && dayOfWeek != result.Weekday()) ||
		(dayIsSet && day != result.Day()) ||
		(monthIsSet && month != result.Month()) ||
		(yearIsSet && year != result.Year()) {
		var expected []string
		if dayOfWeekIsSet {
			expected = append(expected, dayOfWeek.String())
		}
		if dayIsSet {
			expected = append(expected, fmt.Sprintf("%d", day))
		}
		if monthIsSet {
			expected = append(expected, month.String())
		}
		if yearIsSet {
			expected = append(expected, fmt.Sprintf("%d", year))
		}
		return nil, fmt.Errorf("no such date: %s", strings.Join(expected, " "))
	}

	// Make sure any implied DST is correct
	if *strictDST &&
		sourceTimezone.dstEnforce &&
		result.IsDST() != sourceTimezone.dstExpected {
		return nil, errDSTMismatch
	}

	if *debug {
		log.Println("result:", result)
	}

	// Apply transformations
	for _, unit := range units {
		if v, ok := unit.(Transform); ok {
			result = v.Apply(result)
			if *debug {
				log.Println("->", result)
			}
		}

		if v, ok := unit.(OutputZone); ok {
			zone, err := ResolveTimezone(v.Zone)
			if err != nil {
				return nil, nil
			}

			if timeIsSet {
				result = result.In(zone.loc)
			} else {
				// If a time has not been set yet make sure changing the timezone
				// does not change the time of day.
				year, month, day := result.Date()
				hour, min, sec := result.In(zone.loc).Clock()
				nsec := result.Nanosecond()
				result = time.Date(year, month, day, hour, min, sec, nsec, zone.loc)
			}

			if *strictDST &&
				zone.dstEnforce &&
				zone.dstExpected != result.IsDST() {
				return nil, errDSTMismatch
			}
		}

		if v, ok := unit.(MaybeLocalTimeAssertion); ok {
			if timeIsSet {
				// Assert time
				var hour, min, sec int
				if v.tz != nil {
					tz, err := ResolveTimezone(v.tz)
					if err != nil {
						return nil, err
					}
					inTZ := result.In(tz.loc)
					if *strictDST && tz.dstEnforce && result.IsDST() != tz.dstExpected {
						return nil, errDSTMismatch
					}
					hour, min, sec = inTZ.Clock()
				} else {
					hour, min, sec = result.Clock()
				}
				if hour != v.t.hour || min != v.t.min || sec != v.t.sec {
					return nil, errTimeAssertionFailure
				}
			} else {
				// Set time
				year, month, day := result.Date()
				if v.tz != nil {
					tz, err := ResolveTimezone(v.tz)
					if err != nil {
						return nil, err
					}
					result = time.Date(year, month, day, v.t.hour, v.t.min, v.t.sec, v.t.nsec, tz.loc)
					if *strictDST && tz.dstEnforce && result.IsDST() != tz.dstExpected {
						return nil, errDSTMismatch
					}
				} else {
					result = time.Date(year, month, day, v.t.hour, v.t.min, v.t.sec, v.t.nsec, result.Location())
				}
				timeIsSet = true
			}
		}
	}

	deltaDir := 1
	deltaUnit := None
	for _, unit := range units {
		if m, ok := unit.(OutputMeasure); ok {
			deltaDir = m.Direction
			deltaUnit = m.Unit
		}
	}
	return &Result{
		Time:           result,
		DeltaBase:      now,
		DeltaUnit:      deltaUnit,
		DeltaDirection: deltaDir,
	}, nil
}

func main() {
	textPattern := regexp.MustCompile(`^([^-]|-$|-\d)`)
	var args []string
	var text []string
	for _, arg := range os.Args[1:] {
		if len(text) > 0 || textPattern.MatchString(arg) {
			text = append(text, arg)
		} else {
			args = append(args, arg)
		}
	}
	flag.CommandLine.Parse(args)

	var res *Result
	now := time.Now()

	if len(text) > 0 {
		s := strings.Join(text, " ")

		var err error
		res, err = parse(now, s)

		if err != nil {
			fmt.Fprintln(os.Stderr, err.Error())
			os.Exit(1)
		}
	} else {
		res = &Result{Time: now}
	}

	var input = res.Time
	var locationString string
	if input.Location() != time.Local && input.Location() != time.UTC {
		locationString = input.Location().String()
	}

	output := input.Format(*layout)
	local := input.Local().Format(*layout)
	utc := input.UTC().Format(time.RFC3339Nano)
	unix := input.Unix()
	unixMillis := input.UnixNano() / 1_000_000
	unixMicros := input.UnixNano() / 1_000
	unixNanos := input.UnixNano()
	deltaOutput := ""

	if res.DeltaUnit != None {
		start, end := res.DeltaBase, res.Time
		if res.DeltaDirection < 0 {
			start, end = end, start
		}
		deltaOutput = res.DeltaUnit.FormatDuration(start, end)
	}

	type jsonOut struct {
		Output     string `json:"output"`
		Local      string `json:"local"`
		UTC        string `json:"utc"`
		Unix       int64  `json:"unix"`
		UnixMillis int64  `json:"unix_millis"`
		UnixMicros int64  `json:"unix_micros"`
		UnixNanos  int64  `json:"unix_nanos"`
		Location   string `json:"location,omitempty"`
		Delta      string `json:"delta,omitempty"`
	}

	if *jsonOutput {
		out := jsonOut{
			Output:     output,
			Local:      local,
			UTC:        utc,
			Unix:       unix,
			UnixMillis: unixMillis,
			UnixMicros: unixMicros,
			UnixNanos:  unixNanos,
			Location:   locationString,
			Delta:      deltaOutput,
		}
		if err := json.NewEncoder(os.Stdout).Encode(out); err != nil {
			log.Fatal(err)
		}
		return
	}

	if deltaOutput != "" {
		fmt.Println(deltaOutput)
		return
	}

	if locationString != "" {
		fmt.Println(output, locationString)
	} else {
		fmt.Println(output)
	}
	fmt.Println(local)
	fmt.Println(utc)
	fmt.Printf("s\t%d\n", unix)
	fmt.Printf("ms\t%d\n", unixMillis)
	fmt.Printf("µs\t%d\n", unixMicros)
	fmt.Printf("ns\t%d\n", unixNanos)
}

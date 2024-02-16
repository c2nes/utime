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
var until = flag.Bool("until", false, "print time until (or since) the resolved time")
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
	Last Keyword = iota
	Next
	At
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

type DeltaPrefix int

const (
	Plus  DeltaPrefix = 1
	Minus DeltaPrefix = -1
)

type Unit int

const (
	UnitNanosecond Unit = iota
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

type QuantityOrOffset string

type ImpliedLocation *time.Location

type ZoneOffset struct {
	Hours   int64
	Minutes int64
	Seconds int64
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

func Is(want any) func(v any) bool {
	return func(v any) bool {
		return v == want
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
					return
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

func parse(now time.Time, s string) (time.Time, error) {
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
		`(?P<year>\d{4})/(?P<month>\d{2})/(?<day>\d{2})`,
		`(?P<day>\d{2})\.(?P<month>\d{2})\.(?<year>\d{4})`,
		`(?P<daymonth>\d{2})/(?P<daymonth>\d{2})/(?<year>\d{4})`,
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
					return time.Time{}, errors.New("ambiguous day/month")
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
				return time.Time{}, err
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
			return time.Time{}, fmt.Errorf("can not parse %q", token)
		}
	}

	var err error
	units, err = applyRules(units,
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
		NewRule(
			Match(IsQuantityOrOffset),
			func(xs []any) ([]any, error) {
				off := string(xs[0].(QuantityOrOffset))
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
				return []any{ZoneOffset{h, m, s}}, nil
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
	)
	if err != nil {
		return time.Time{}, err
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
				return time.Time{}, nil
			}
			sourceTimezoneIsExplicit = true
		}
		if zone, ok := unit.(ImpliedLocation); ok && !sourceTimezoneIsExplicit {
			var err error
			sourceTimezone, err = ResolveTimezone((*time.Location)(zone))
			if err != nil {
				return time.Time{}, nil
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
		var out []string
		for _, unit := range units {
			out = append(out, fmt.Sprintf("%T(%#v)", unit, unit))
		}
		fmt.Println(strings.Join(out, ","))
	}

	// Check for bare quantities and other errors
	for _, unit := range units {
		if _, ok := unit.(Unit); ok {
			return time.Time{}, errors.New("bare unit")
		}
		if _, ok := unit.(DeltaPrefix); ok {
			return time.Time{}, errors.New("unexpected +/-")
		}
		if _, ok := unit.(DeltaSuffix); ok {
			return time.Time{}, errors.New("unexpected before/after")
		}
		if v, ok := unit.(*Delta); ok {
			if v.Direction == 0 {
				return time.Time{}, errors.New("delta without direction (missing after/before?)")
			}
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
		return time.Time{}, errors.New("multiple times specified")
	}
	if count(units, IsDay) > 1 {
		return time.Time{}, errors.New("multiple days specified")
	}
	if count(units, IsMonth) > 1 {
		return time.Time{}, errors.New("multiple months specified")
	}
	if count(units, IsYear) > 1 {
		return time.Time{}, errors.New("multiple years specified")
	}
	if count(units, IsTimeOfDay) > 0 {
		return time.Time{}, errors.New("multiple times of day specified")
	}
	if count(units, IsDayOfWeek) > 1 {
		return time.Time{}, errors.New("multiple days of the week specified")
	}
	if count(units, IsTimezone) > 1 {
		return time.Time{}, errors.New("multiple timezones specified")
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
	}

	if yearIsSet && (!monthIsSet && !dayIsSet) {
		// Year only. Set to Jan 1.
		month = time.January
		day = 1
	} else if monthIsSet && !dayIsSet {
		// Month, but no day. Set to the 1st.
		day = 1
	}

	// If any date component is specified, but no time is given
	// then default time to midnight.
	if !timeIsSet && (yearIsSet || monthIsSet || dayIsSet) {
		hour = 0
		min = 0
		sec = 0
		nsec = 0
	}

	// TODO: Handle ambiguous times at DST boundaries when an hour occurs twice
	result := time.Date(year, month, day, hour, min, int(sec), int(nsec), sourceTimezone.loc)

	// We now have a fully specified base time. If a day of week was
	// given, make sure it is correct.
	for _, unit := range units {
		if v, ok := unit.(DayOfWeek); ok {
			if result.Weekday() != time.Weekday(v) {
				return time.Time{}, errors.New("day of week does not match")
			}
		}
	}

	// Make sure any implied DST is correct
	if *strictDST &&
		sourceTimezone.dstEnforce &&
		result.IsDST() != sourceTimezone.dstExpected {
		return time.Time{}, errDSTMismatch
	}

	// Apply deltas
	for _, unit := range units {
		if v, ok := unit.(*Delta); ok {
			result = v.Apply(result)
		}

		if v, ok := unit.(OutputZone); ok {
			zone, err := ResolveTimezone(v.Zone)
			if err != nil {
				return time.Time{}, nil
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
				return time.Time{}, errDSTMismatch
			}
		}

		if v, ok := unit.(MaybeLocalTimeAssertion); ok {
			if timeIsSet {
				// Assert time
				var hour, min, sec int
				if v.tz != nil {
					tz, err := ResolveTimezone(v.tz)
					if err != nil {
						return time.Time{}, err
					}
					inTZ := result.In(tz.loc)
					if *strictDST && tz.dstEnforce && result.IsDST() != tz.dstExpected {
						return time.Time{}, errDSTMismatch
					}
					hour, min, sec = inTZ.Clock()
				} else {
					hour, min, sec = result.Clock()
				}
				if hour != v.t.hour || min != v.t.min || sec != v.t.sec {
					return time.Time{}, errTimeAssertionFailure
				}
			} else {
				// Set time
				year, month, day := result.Date()
				if v.tz != nil {
					tz, err := ResolveTimezone(v.tz)
					if err != nil {
						return time.Time{}, err
					}
					result = time.Date(year, month, day, v.t.hour, v.t.min, v.t.sec, v.t.nsec, tz.loc)
					if *strictDST && tz.dstEnforce && result.IsDST() != tz.dstExpected {
						return time.Time{}, errDSTMismatch
					}
				} else {
					result = time.Date(year, month, day, v.t.hour, v.t.min, v.t.sec, v.t.nsec, result.Location())
				}
				timeIsSet = true
			}
		}
	}

	return result, nil
}

func main() {
	var args []string
	var text []string
	for _, arg := range os.Args[1:] {
		if len(text) > 0 || !strings.HasPrefix(arg, "-") {
			text = append(text, arg)
		} else {
			args = append(args, arg)
		}
	}
	flag.CommandLine.Parse(args)

	var input time.Time
	now := time.Now()

	if len(text) > 0 {
		s := strings.Join(text, " ")

		var err error
		input, err = parse(now, s)

		if err != nil {
			fmt.Fprintln(os.Stderr, err.Error())
			os.Exit(1)
		}
	} else {
		input = now
	}

	if *until {
		fmt.Println(input.Sub(now))
		return
	}

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

	type jsonOut struct {
		Output     string
		Local      string
		UTC        string
		Unix       int64
		UnixMillis int64
		UnixMicros int64
		UnixNanos  int64
		Location   string
	}

	if *jsonOutput {
		out := jsonOut{output, local, utc, unix, unixMillis, unixMicros, unixNanos, locationString}
		if err := json.NewEncoder(os.Stdout).Encode(out); err != nil {
			log.Fatal(err)
		}
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

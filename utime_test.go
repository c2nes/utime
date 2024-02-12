package main

import (
	"testing"
	"time"
)

func TestParse(t *testing.T) {
	*preferDDMM = true
	layouts := []string{
		time.Layout,
		time.ANSIC,
		time.UnixDate,
		time.RubyDate,
		time.RFC822,
		time.RFC822Z,
		time.RFC850,
		time.RFC1123,
		time.RFC1123Z,
		time.RFC3339,
		time.RFC3339Nano,
		time.Kitchen,
		time.Stamp,
		time.StampMilli,
		time.StampMicro,
		time.StampNano,
		time.DateTime,
		time.DateOnly,
		time.TimeOnly,
	}

	now := time.Now()
	for _, layout := range layouts {
		s := now.Format(layout)
		expected, err := time.Parse(layout, s)
		if err != nil {
			panic(err)
		}
		zero, _ := time.Parse("", "")
		actual, err := parse(zero, s)
		if err != nil {
			t.Errorf("failed to parse %q: %v", s, err)
			continue
		}
		if expected.Second() != actual.Second() {
			t.Errorf("S: %v (expected) != %v (actual); %q", expected, actual, s)
		}
		if expected.Nanosecond() != actual.Nanosecond() {
			t.Errorf("N: %v (expected) != %v (actual); %q", expected.Nanosecond(), actual.Nanosecond(), s)
		}
	}
}

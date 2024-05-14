package main

import (
	"bufio"
	"fmt"
	"go/format"
	"log"
	"os"
	"regexp"
	"slices"
	"strings"
	"time"
	_ "time/tzdata"
)

func readFileLines(filename string) (lines []string, err error) {
	f, err := os.Open(filename)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	b := bufio.NewScanner(f)
	for b.Scan() {
		lines = append(lines, b.Text())
	}
	err = b.Err()
	return
}

func isZoneSupported(zone string) bool {
	_, err := time.LoadLocation(zone)
	return err == nil
}

func iso3166() (map[string]string, error) {
	lines, err := readFileLines("build/iso3166.tab")
	if err != nil {
		return nil, err
	}
	out := make(map[string]string)
	for _, l := range lines {
		if strings.HasPrefix(l, "#") {
			continue
		}
		parts := strings.Split(l, "\t")
		out[parts[0]] = parts[1]
	}
	return out, nil
}

type zoneTabEntry struct {
	cc []string
	tz string

	comment string
}

func zone1970() ([]*zoneTabEntry, error) {
	lines, err := readFileLines("build/zone1970.tab")
	if err != nil {
		return nil, err
	}
	var out []*zoneTabEntry
	for _, l := range lines {
		if strings.HasPrefix(l, "#") {
			continue
		}
		parts := strings.Split(l, "\t")
		// country-code, coordinates, TZ, comments
		cc := strings.Split(parts[0], ",")
		tz := parts[2]
		comment := ""
		if len(parts) == 4 {
			comment = parts[3]
		}
		out = append(out, &zoneTabEntry{cc, tz, comment})
	}
	return out, nil
}

// Returns a map of country code to that country's only -- or principle -- zone.
func singleZoneCountries() (map[string]string, error) {
	zoneTab, err := zone1970()
	if err != nil {
		return nil, err
	}

	zonesByCC := make(map[string][]string)
	for _, e := range zoneTab {
		for _, cc := range e.cc {
			zonesByCC[cc] = append(zonesByCC[cc], e.tz)
		}
	}
	// Some countries have a "mainland" designated in the zonetab file.
	// Prefer those zones when available.
	for _, e := range zoneTab {
		if len(e.cc) == 1 && strings.Contains(e.comment, "(mainland)") {
			zonesByCC[e.cc[0]] = []string{e.tz}
		}
	}

	// Hard code some countries with a clear "default" timezone
	zonesByCC["CN"] = []string{"Asia/Shanghai"}

	// When countries have multiple timezones we compare the zones at various
	// points in time between 2010 and 2024. If they agree at all instants
	// then we consider the zones to be equivalent and we select the first
	// as the canonical zone.
	var trialInstants []time.Time
	trialInstant := time.Date(2010, time.January, 1, 0, 0, 0, 0, time.UTC)
	for trialInstant.Year() < 2024 {
		trialInstants = append(trialInstants, trialInstant)
		trialInstant = trialInstant.Add(time.Hour * 23)
	}

	// country code to zone
	out := make(map[string]string)
	for cc, zones := range zonesByCC {
		if len(zones) == 1 {
			out[cc] = zones[0]
			continue
		}
		var locs []*time.Location
		for _, z := range zones {
			loc, err := time.LoadLocation(z)
			if err != nil {
				return nil, err
			}
			locs = append(locs, loc)
		}
		zonesAgree := true
	TestAgreement:
		for _, ti := range trialInstants {
			const layout = "2006-01-02T15:04:05"
			reference := ti.In(locs[0]).Format(layout)
			for _, l := range locs[1:] {
				if ti.In(l).Format(layout) != reference {
					zonesAgree = false
					break TestAgreement
				}
			}
		}
		if zonesAgree {
			out[cc] = zones[0]
		}
	}
	return out, nil
}

func main() {
	tzdata, err := readFileLines("build/tzdata.zi")
	if err != nil {
		panic(err)
	}
	reZ := regexp.MustCompile(`^Z\s+([^\s]+)`)
	reL := regexp.MustCompile(`^L\s+([^\s]+)\s+([^\s]+)`)
	unique := make(map[string]bool)
	var zones []string
	aliases := make(map[string]string)
	cities := make(map[string]string)

	for _, l := range tzdata {
		if m := reZ.FindStringSubmatch(l); m != nil {
			z := m[1]
			if !isZoneSupported(z) {
				log.Printf("[WARN] Omitting unsupported zone %q", z)
				continue
			}
			unique[strings.ToLower(z)] = true
			zones = append(zones, fmt.Sprintf("%q: %q,", strings.ToLower(z), z))
			cityIdx := strings.LastIndexByte(z, '/') + 1
			if cityIdx > 0 {
				city := strings.ToLower(z[cityIdx:])
				cities[city] = fmt.Sprintf("%q: %q,", city, z)
			}
		} else if m := reL.FindStringSubmatch(l); m != nil {
			alias := strings.ToLower(m[2])
			zone := m[1]
			if !isZoneSupported(zone) {
				log.Printf("[WARN] Omitting unsupported zone %q", zone)
				continue
			}
			aliases[alias] = fmt.Sprintf("%q: %q,", alias, zone)
		}
	}

	var aliasLines []string
	for key, line := range aliases {
		if unique[key] {
			continue
		}
		unique[key] = true
		aliasLines = append(aliasLines, line)
	}

	var cityLines []string
	for key, line := range cities {
		if unique[key] {
			continue
		}
		unique[key] = true
		cityLines = append(cityLines, line)
	}

	ccNames, err := iso3166()
	if err != nil {
		panic(err)
	}
	ccZones, err := singleZoneCountries()
	if err != nil {
		panic(err)
	}
	var countryLines []string
	for cc, zone := range ccZones {
		country := ccNames[cc]
		if strings.ContainsRune(country, ' ') {
			log.Printf("Skipping zone alias for %q -> %q", country, zone)
			continue
		}
		key := strings.ToLower(country)
		if unique[key] {
			continue
		}
		unique[key] = true
		countryLines = append(countryLines, fmt.Sprintf("%q: %q,", key, zone))
	}

	slices.Sort(zones)
	slices.Sort(aliasLines)
	slices.Sort(cityLines)
	slices.Sort(countryLines)
	lines := []string{
		`package main`,
		`var Zones = map[string]string {`,
	}
	lines = append(lines, `// Zones`)
	lines = append(lines, zones...)
	lines = append(lines, `// Aliases`)
	lines = append(lines, aliasLines...)
	lines = append(lines, `// Cities`)
	lines = append(lines, cityLines...)
	lines = append(lines, `// Countries`)
	lines = append(lines, countryLines...)
	lines = append(lines, `}`)
	src := strings.Join(lines, "\n")
	out, err := format.Source([]byte(src))
	if err != nil {
		panic(err)
	}
	if err := os.WriteFile("zones.go", out, 0660); err != nil {
		panic(err)
	}
}

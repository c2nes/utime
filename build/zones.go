package main

import (
	"bufio"
	"fmt"
	"go/format"
	"io"
	"os"
	"regexp"
	"slices"
	"strings"
)

func main() {
	f, err := os.Open("build/tzdata.zi")
	if err != nil {
		panic(err)
	}
	defer f.Close()
	r := bufio.NewReader(f)
	reZ := regexp.MustCompile(`^Z\s+([^\s]+)`)
	reL := regexp.MustCompile(`^L\s+([^\s]+)\s+([^\s]+)`)
	var zones []string
	aliases := make(map[string]string)
	cities := make(map[string]string)

	for err != io.EOF {
		var l string
		l, err = r.ReadString('\n')
		if err != nil && err != io.EOF {
			panic(err)
		}
		if m := reZ.FindStringSubmatch(l); m != nil {
			z := m[1]
			zones = append(zones, fmt.Sprintf("%q: %q,", strings.ToLower(z), z))
			cityIdx := strings.LastIndexByte(z, '/') + 1
			if cityIdx > 0 {
				city := strings.ToLower(z[cityIdx:])
				cities[city] = fmt.Sprintf("%q: %q,", city, z)
			}
		} else if m := reL.FindStringSubmatch(l); m != nil {
			alias := strings.ToLower(m[2])
			zone := m[1]
			aliases[alias] = fmt.Sprintf("%q: %q,", alias, zone)
		}
	}

	var aliasLines []string
	for _, line := range aliases {
		aliasLines = append(aliasLines, line)
	}

	var cityLines []string
	for city, line := range cities {
		if aliases[city] == "" {
			cityLines = append(cityLines, line)
		}
	}

	slices.Sort(zones)
	slices.Sort(aliasLines)
	slices.Sort(cityLines)
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

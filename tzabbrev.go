package main

type ZoneAbbrev struct {
	Abbev string
	IsDST bool
	Zone  string
}

var ZoneAbbrevs = map[string]ZoneAbbrev{
	"SAST": {"SAST", false, "Africa/Johannesburg"},
	"WAT":  {"WAT", false, "Africa/Lagos"},
	"CAT":  {"CAT", false, "Africa/Lusaka"},
	"EAT":  {"EAT", false, "Africa/Nairobi"},
	"HDT":  {"HDT", true, "America/Adak"},
	"HST":  {"HST", false, "America/Adak"},
	"AKDT": {"AKDT", true, "America/Anchorage"},
	"AKST": {"AKST", false, "America/Anchorage"},
	"CDT":  {"CDT", true, "America/Chicago"},
	"CST":  {"CST", false, "America/Chicago"},
	"MDT":  {"MDT", true, "America/Denver"},
	"MST":  {"MST", false, "America/Denver"},
	"ADT":  {"ADT", true, "America/Halifax"},
	"AST":  {"AST", false, "America/Halifax"},
	"PDT":  {"PDT", true, "America/Los_Angeles"},
	"PST":  {"PST", false, "America/Los_Angeles"},
	"EDT":  {"EDT", true, "America/New_York"},
	"EST":  {"EST", false, "America/New_York"},
	"NDT":  {"NDT", true, "America/St_Johns"},
	"NST":  {"NST", false, "America/St_Johns"},
	"HKT":  {"HKT", false, "Asia/Hong_Kong"},
	"WIB":  {"WIB", false, "Asia/Jakarta"},
	"WIT":  {"WIT", false, "Asia/Jayapura"},
	"PKT":  {"PKT", false, "Asia/Karachi"},
	"IST":  {"IST", false, "Asia/Kolkata"},
	"WITA": {"WITA", false, "Asia/Makassar"},
	"KST":  {"KST", false, "Asia/Seoul"},
	"JST":  {"JST", false, "Asia/Tokyo"},
	"ACDT": {"ACDT", true, "Australia/Adelaide"},
	"ACST": {"ACST", false, "Australia/Adelaide"},
	"AWST": {"AWST", false, "Australia/Perth"},
	"AEDT": {"AEDT", true, "Australia/Sydney"},
	"AEST": {"AEST", false, "Australia/Sydney"},
	"EEST": {"EEST", true, "Europe/Kyiv"},
	"EET":  {"EET", false, "Europe/Kyiv"},
	"WEST": {"WEST", true, "Europe/Lisbon"},
	"WET":  {"WET", false, "Europe/Lisbon"},
	"BST":  {"BST", true, "Europe/London"},
	"GMT":  {"GMT", false, "Europe/London"},
	"MSK":  {"MSK", false, "Europe/Moscow"},
	"CEST": {"CEST", true, "Europe/Paris"},
	"CET":  {"CET", false, "Europe/Paris"},
	"NZDT": {"NZDT", true, "Pacific/Auckland"},
	"NZST": {"NZST", false, "Pacific/Auckland"},
	"ChST": {"ChST", false, "Pacific/Guam"},
	"SST":  {"SST", false, "Pacific/Midway"},
}

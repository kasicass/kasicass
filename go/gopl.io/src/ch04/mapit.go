package main

import (
	"fmt"
	"sort"
)

func main() {
	ages := map[string]int{
		"alice":   31,
		"charlie": 34,
		"kong":    33,
		"phay":    4,
		"baby":    7,
	}

	fmt.Println(len(ages))
	fmt.Println(ages["alice"], ages["none"])
	fmt.Println(len(ages))
	ages["none"] += 1
	fmt.Println(len(ages))

	for name, age := range ages {
		fmt.Printf("%s\t%d\n", name, age)
	}
	fmt.Println("-------")

	var names []string
	for name := range ages {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
		fmt.Printf("%s\t%d\n", name, ages[name])
	}
}

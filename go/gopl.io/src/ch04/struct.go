package main

import (
	"fmt"
)

type Point struct {
	X, Y int
}

type address struct {
	hostname string
	port int
}

func main() {
	p := Point{1, 2}
	q := Point{2, 1}
	fmt.Println(p.X == q.X && p.Y == q.Y)
	fmt.Println(p == q)

	hits := make(map[address]int)
	hits[address{"golang.org", 443}]++
}

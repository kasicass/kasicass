package main

import (
	"fmt"
)

func incr(p *int) int {
	*p++
	return *p
}

func main() {
	x := 1
	fmt.Println(x, &x)

	incr(&x)
	fmt.Println(incr(&x))
}

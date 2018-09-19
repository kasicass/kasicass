package main

import "fmt"

func foo() {}
func bar() {}

var g = "g"

func main() {
	foo := "f"
	fmt.Println(foo)
	fmt.Println(bar)
	fmt.Println(g)
}
package main

import (
	"fmt"
)

func main() {
	x := struct{}
	y := struct{}
	fmt.Println(&x == &y)
}
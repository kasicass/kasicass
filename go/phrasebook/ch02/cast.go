package main

import (
	"fmt"
)

type empty interface {}
type example interface {
	notImplemented()
}

func main() {
	one := 1
	var i empty = one
	var float float32
	float = float32(one)
	switch i.(type) {
	default:
		fmt.Printf("Type error!\n")
	case int:
		fmt.Printf("%d\n", i)
	}
	fmt.Printf("%f\n", float)

	// This will panic at run time
	var e example = i.(example)
	fmt.Printf("%d\n", e.(empty).(int))
}


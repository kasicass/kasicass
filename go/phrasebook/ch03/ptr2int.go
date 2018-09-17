package main

import (
	"fmt"
	"unsafe"
)

func main() {
	str := "A Go variable"
	addr := unsafe.Pointer(&str)
	fmt.Printf("The address of str is %d\n", addr)
	str2 := (*string)(addr)
	fmt.Printf("String constructed from pointer: %s\n", *str2)
	address := uintptr(addr)
	address += 4
	// This has undefined behavior!
	str3 := (*string)(unsafe.Pointer(address))
	fmt.Printf("String construct from pointer: %s\n", *str3)
}


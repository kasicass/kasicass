package main

import "fmt"

func main() {
	fmt.Println(basename("a/b/c.go"))
	fmt.Println(basename("a.b.go"))
	fmt.Println(basename("abc"))
}

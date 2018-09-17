package main

import (
	"fmt"
	"strconv"
)

func main() {
	var i int
	fmt.Scanf("%d", &i)
	str := strconv.FormatInt(int64(i), 10)
	hex, _ := strconv.ParseInt(str, 16, 64)
	fmt.Printf("%d\n", hex)
}


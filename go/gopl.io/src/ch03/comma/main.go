package main

import (
	"fmt"
	"bytes"
)

func comma(s string) string {
	n := len(s)
	if n <= 3 {
		return s
	}
	return comma(s[:n-3]) + "," + s[n-3:]
}

func comma2(s string) string {
	n := len(s)
	if n <= 3 {
		return s
	}

	var buf bytes.Buffer
	begin := n % 3
	if begin > 0 {
		buf.WriteString(s[:begin])
		buf.WriteByte(',')
	}

	step := n / 3
	i := 0
	for i < step-1 {
		buf.WriteString(s[begin+i*3:begin+(i+1)*3])
		buf.WriteByte(',')
		i++
	}
	buf.WriteString(s[begin+i*3:])
	return buf.String()
}

func main() {
	fmt.Println(comma("1234567"))
	fmt.Println(comma2("1234567"))
}

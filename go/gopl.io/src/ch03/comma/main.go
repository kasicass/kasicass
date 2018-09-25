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

func anagram(s1, s2 string) bool {
	len1 := len(s1)
	len2 := len(s2)
	if len1 != len2 {
		return false
	}

	for i := 0; i < len1; i++ {
		if s1[i] != s2[len2-1-i] {
			return false
		}
	}

	return true
}

func main() {
	fmt.Println(comma("1234567"))
	fmt.Println(comma2("1234567"))
	fmt.Println(anagram("hello1", "2olleh"))
	fmt.Println(anagram("hello", "olleh"))
}

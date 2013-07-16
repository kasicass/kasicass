package sum

func Sum(values []int) int {
	s := 0
	for _, v := range values {
		s += v
	}
	return s
}


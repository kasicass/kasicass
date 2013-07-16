package sum

import "testing"

func TestSum(t *testing.T) {
	values := []int{1,2,3}
	s := Sum(values)
	if s != 6 {
		t.Error("Sum failed", s, "expected 6")
	}
}


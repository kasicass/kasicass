package main

import (
	"fmt"
	"math"
)

func main() {
	fmt.Println(math.MaxFloat32)
	fmt.Println(math.MaxFloat64)

	for x := 0; x < 8; x++ {
		fmt.Printf("x = %d e^x = %8.3f\n", x, math.Exp(float64(x)))
	}

	nan := math.NaN()
	fmt.Println(nan == nan, nan < nan, nan > nan)
}

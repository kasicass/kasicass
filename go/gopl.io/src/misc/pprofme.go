// go tool pprof http://localhost:8000/debug/pprof/heap
// browser: http://localhost:8000/debug/pprof/

package main

import (
	"fmt"
	"log"
	"math/rand"
	"net/http"
	_ "net/http/pprof"
	"time"
)

func busyWork() {
	v := make([]int, 10000000)
	i := 0
	vcap := cap(v)
	for i < vcap {
		v = append(v, rand.Int())
		i++
	}
}

func main() {
	go func() {
		log.Println(http.ListenAndServe("localhost:8000", nil))
	}()

	for {
		start := time.Now()
		busyWork()
		t := time.Now()
		elapsed := t.Sub(start)

		fmt.Printf("elapsed: %q\n", elapsed)
		time.Sleep(elapsed)
	}
}

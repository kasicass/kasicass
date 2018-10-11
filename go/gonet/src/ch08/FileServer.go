package main

import (
	"fmt"
	"net/http"
	"os"
)

func main() {
	fileServer := http.FileServer(http.Dir('.'))

	err := http.ListenAndServe(":8000", fileServer)
	checkError(err)
}

func checkError(err error) {
	if err != nil {
		fmt.Fprintf(os.Stderr, "Fatal error: %s", err.Error())
		os.Exit(1)
	}
}

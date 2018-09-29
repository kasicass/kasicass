package main

import (
	"os"
	"log"
)

func main() {
	file, err := os.Open("file_not_exists.go")
	if err != nil {
		log.Fatal(err)
	}
	file.Close()
}
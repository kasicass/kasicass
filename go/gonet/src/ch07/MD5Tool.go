package main

import (
	"crypto/md5"
	"fmt"
	"os"
	"io"
)

func main() {
	if len(os.Args) != 2 {
		fmt.Println("Usage:", os.Args[0], "<filename>")
		os.Exit(1)
	}

	filename := os.Args[1]
	f, err := os.Open(filename)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		os.Exit(1)
	}
	defer f.Close()

	/*
	data, err := ioutil.ReadAll(f)
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		os.Exit(1)
	}
	*/

	hash := md5.New()
	for {
		var buf [2048]byte
		n, err := f.Read(buf[0:])
		if err != nil {
			if err == io.EOF {
				break
			}
			fmt.Printf("Error: %s\n", err.Error())
			os.Exit(1)
		}
		hash.Write(buf[:n])
	}

	hashValue := hash.Sum(nil)
	hashSize := hash.Size()
	for n := 0; n < hashSize; n += 4 {
		var val uint32
		val = uint32(hashValue[n])<<24 +
				uint32(hashValue[n+1])<<16 +
				uint32(hashValue[n+2])<<8 +
				uint32(hashValue[n+3])
		fmt.Printf("%x ", val)
	}
}

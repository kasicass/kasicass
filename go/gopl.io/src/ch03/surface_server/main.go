package main

import (
//	"fmt"
	"log"
	"net/http"

	surface "ch03/surface"
)

func main() {
	http.HandleFunc("/", handler)
	log.Fatal(http.ListenAndServe("localhost:8000", nil))
}

func handler(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "image/svg+xml")
	surface.Surface(w)
	// fmt.Fprintf(w, "URL.Path = %q\n", r.URL.Path)
}

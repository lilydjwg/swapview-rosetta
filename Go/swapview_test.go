package main

import (
	"testing"
)

func BenchmarkGetInfos(t *testing.B) {
	t.ResetTimer()
	for i := 0; i < t.N; i++ {
		GetInfos()
	}
}

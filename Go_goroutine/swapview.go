package main

import (
	"bufio"
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"sort"
	"strconv"
	"strings"
	"sync"
	// "time"
)

type Info struct {
	Pid  int
	Size int64
	Comm string
}

var (
	nullBytes  = []byte{0x0}
	emptyBytes = []byte(" ")
	swapPrefix = "Swap:"
)

func main() {
	slist := GetInfos()
	sort.Slice(slist, func(i, j int) bool {
		return slist[i].Size < slist[j].Size
	})

	fmt.Printf("%7s %9s %s\n", "PID", "SWAP", "COMMAND")
	var total int64
	for _, v := range slist {
		fmt.Printf("%7d %9s %s\n", v.Pid, FormatSize(v.Size), v.Comm)
		total += v.Size
	}
	fmt.Printf("Total: %10s\n", FormatSize(total))
}

func GetInfos() (list []Info) {
	f, _ := os.Open("/proc")
	defer f.Close()
	names, err := f.Readdirnames(0)
	if err != nil {
		log.Fatalf("read /proc: %v", err)
	}

	length := len(names)

	list = make([]Info, 0, length)
	infoCh := make(chan Info, length)

	wg := &sync.WaitGroup{}
	wg.Add(length)

	go func() {
		defer close(infoCh)
		defer wg.Wait()

		for _, name := range names {
			go GetInfo(name, infoCh, wg)
		}
	}()

	for v := range infoCh {
		list = append(list, v)
	}
	return
}

func GetInfo(name string, infoCh chan<- Info, wg *sync.WaitGroup) {
	defer wg.Done()

	pid, err := strconv.Atoi(name)
	if err != nil {
		return
	}

	info := Info{
		Pid: pid,
	}

	var bs []byte
	bs, err = ioutil.ReadFile(fmt.Sprintf("/proc/%d/cmdline", pid))
	if err != nil {
		return
	}
	if bytes.HasSuffix(bs, nullBytes) {
		bs = bs[:len(bs)-1]
	}
	info.Comm = string(bytes.Replace(bs, nullBytes, emptyBytes, -1))

	bs, err = ioutil.ReadFile(fmt.Sprintf("/proc/%d/smaps", pid))
	if err != nil {
		return
	}

	var total, size int64
	var b string

	r := bufio.NewScanner(bytes.NewReader(bs))
	for r.Scan() {
		b = r.Text()
		if !strings.HasPrefix(b, swapPrefix) {
			continue
		}

		x := strings.Split(b, string(emptyBytes))
		size, err = strconv.ParseInt(x[len(x)-2], 10, 64)
		if err != nil {
			return
		}

		total += size
	}

	// No swap pid info should be ignored.
	if total == 0 {
		return
	}
	info.Size = total * 1024
	infoCh <- info
	return
}

var units = []string{"", "K", "M", "G", "T"}

func FormatSize(s int64) string {
	if s <= 1100 {
		return fmt.Sprintf("%dB", s)
	}

	unit := 0
	f := float64(s)
	for unit < len(units) && f > 1100.0 {
		f /= 1024.0
		unit++
	}
	return fmt.Sprintf("%.1f%siB", f, units[unit])
}

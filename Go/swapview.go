package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"sort"
	"strconv"
        "strings"
        // "time"
)

type Info struct {
	Pid  int
	Size int64
	Comm string
}

type Infos []Info

func (p Infos) Len() int           { return len(p) }
func (p Infos) Less(i, j int) bool { return p[i].Size < p[j].Size }
func (p Infos) Swap(i, j int)      { p[i], p[j] = p[j], p[i] }

func main() {
        // t0 := time.Now()
        // defer func() {
        //         fmt.Printf("%v\n", time.Now().Sub(t0))
        // }()

	slist := GetInfos()
	sort.Sort(Infos(slist))

	fmt.Printf("%5s %9s %s\n", "PID", "SWAP", "COMMAND")
	var total int64
	for _, v := range slist {
		fmt.Printf("%5d %9s %s\n", v.Pid, FormatSize(v.Size), v.Comm)
		total += v.Size
	}
	fmt.Printf("Total: %8s\n", FormatSize(total))
}

func GetInfos() (list []Info) {
	f, _ := os.Open("/proc")
	defer f.Close()
	names, err := f.Readdirnames(0)
	if err != nil {
		log.Fatalf("read /proc: %v", err)
	}
	for _, name := range names {
		pid, err := strconv.Atoi(name)
		if err != nil {
			continue
		}
		info, err := GetInfo(pid)
		if err != nil || info.Size == 0 {
			continue
		}
		list = append(list, info)
	}
	return
}

func GetInfo(pid int) (info Info, err error) {
	info.Pid = pid
	var bs []byte
	bs, err = ioutil.ReadFile(fmt.Sprintf("/proc/%d/cmdline", pid))
	if err != nil {
		return
	}
        var comm = string(bs)
        if strings.HasSuffix(comm, "\x00") {
            comm = comm[:len(comm)-1]
        }
	info.Comm = strings.Replace(comm, "\x00", " ", -1)
	bs, err = ioutil.ReadFile(fmt.Sprintf("/proc/%d/smaps", pid))
	if err != nil {
		return
	}
	var total int64
	for _, line := range bytes.Split(bs, []byte("\n")) {
		if bytes.HasPrefix(line, []byte("Swap:")) {
			start := bytes.IndexAny(line, "0123456789")
			end := bytes.Index(line[start:], []byte(" "))
			size, err := strconv.ParseInt(string(line[start:start+end]), 10, 0)
			if err != nil {
				continue
			}
			total += size
		}
	}
	info.Size = total
	return
}

var units = []string{"K", "M", "G", "T"}

func FormatSize(s int64) string {
	unit := 0
	f := float64(s)
	for unit < len(units) && f > 1024.0 {
		f /= 1024.0
		unit++
	}
	return fmt.Sprintf("%.1f%siB", f, units[unit])
}

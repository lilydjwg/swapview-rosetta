package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"os"
	"reflect"
	"sort"
	"strconv"
	"unsafe"
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
	var buf bytes.Buffer
	for _, name := range names {
		pid, err := strconv.Atoi(name)
		if err != nil {
			continue
		}
		info, err := GetInfo(pid, &buf)
		if err != nil || info.Size == 0 {
			continue
		}
		list = append(list, info)
	}
	return
}

func GetInfo(pid int, buf *bytes.Buffer) (info Info, err error) {
	info.Pid = pid

	buf.Reset()
	if err = ReadFile(fmt.Sprintf("/proc/%d/cmdline", pid), buf); err != nil {
		return
	}
	comm := buf.Bytes()
	if len(comm) > 0 && comm[len(comm)-1] == 0 {
		comm = comm[:len(comm)-1]
	}
	for i := 0; i < len(comm); i++ {
		if comm[i] == 0 {
			comm[i] = ' '
		}
	}
	info.Comm = string(comm)

	buf.Reset()
	if err = ReadFile(fmt.Sprintf("/proc/%d/smaps", pid), buf); err != nil {
		return
	}
	var total int64
	for _, line := range bytes.Split(buf.Bytes(), []byte("\n")) {
		if bytes.HasPrefix(line, []byte("Swap:")) {
			start := bytes.IndexAny(line[5:], "0123456789")
			end := bytes.Index(line[start:], []byte{' '})
			size, err := strconv.ParseInt(Hack(line[start:start+end]), 10, 0)
			if err != nil {
				continue
			}
			total += size
		}
	}
	info.Size = total
	return
}

func ReadFile(fileName string, buf *bytes.Buffer) error {
	f, err := os.Open(fileName)
	if err != nil {
		return err
	}
	_, err = io.Copy(buf, f)
	f.Close()
	return err
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

func Hack(b []byte) (s string) {
	if len(b) == 0 {
		return ""
	}
	pbytes := (*reflect.SliceHeader)(unsafe.Pointer(&b))
	pstring := (*reflect.StringHeader)(unsafe.Pointer(&s))
	pstring.Data = pbytes.Data
	pstring.Len = pbytes.Len
	return
}

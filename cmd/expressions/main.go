package main

import (
	"bufio"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"strings"

	exprs "github.com/zephyrtronium/expressions"
)

func main() {
	log.SetFlags(0)
	var (
		inname, verb string
		with         [][2]string
		nl, echo     bool
		prec         int
	)
	addwith := func(s string) error {
		d := strings.SplitN(s, "=", 2)
		if len(d) != 2 {
			return fmt.Errorf(`variable definitions must be "name=value", not %q`, s)
		}
		with = append(with, [2]string{strings.TrimSpace(d[0]), strings.TrimSpace(d[1])})
		return nil
	}
	flag.StringVar(&inname, "in", "", "input file (default stdin if no args given)")
	flag.StringVar(&verb, "fmt", "%g", "result formatting string")
	flag.Func("given", "name=value variable definition (any number of times)", addwith)
	flag.IntVar(&prec, "p", 64, "precision of calculations in bits")
	flag.BoolVar(&nl, "n", false, "parse separate input lines as separate expressions")
	flag.BoolVar(&echo, "echo", false, "print parse trees")
	flag.Parse()
	if prec < 0 {
		log.Fatalf("precision (%d) must be positive", prec)
	}

	ins, err := infile(inname, flag.NArg() == 0, nl)
	if err != nil {
		log.Fatal(err)
	}
	for _, arg := range flag.Args() {
		ins = append(ins, strings.NewReader(arg))
	}

	ctx := exprs.NewContext(exprs.Prec(uint(prec)))
	for _, d := range with {
		nm := d[0]
		vl := d[1]
		r, err := exprs.EvalString(vl, exprs.Prec(uint(prec)))
		if err != nil {
			log.Fatalf("setting %s: %v", nm, err)
		}
		ctx.Set(nm, r)
	}

	var p []*exprs.Expr
	for _, in := range ins {
		a, err := exprs.Parse(in)
		if err != nil {
			log.Fatal(err)
		}
		p = append(p, a)
	}

	verb += "\n"
	for _, a := range p {
		if echo {
			fmt.Printf("%v : ", a)
		}
		r := ctx.Eval(a)
		if r == nil {
			fmt.Println(ctx.Err())
			continue
		}
		fmt.Printf(verb, r)
	}
}

func infile(inname string, std, nl bool) ([]io.RuneScanner, error) {
	var f *os.File
	switch {
	case inname != "" && inname != "-":
		in, err := os.Open(inname)
		if err != nil {
			log.Fatal(err)
		}
		f = in
	case inname == "-", std:
		f = os.Stdin
	}
	if f == nil {
		return nil, nil
	}
	if !nl {
		return []io.RuneScanner{bufio.NewReader(f)}, nil
	}
	var r []io.RuneScanner
	scan := bufio.NewScanner(f)
	for scan.Scan() {
		r = append(r, strings.NewReader(scan.Text()))
	}
	return r, scan.Err()
}

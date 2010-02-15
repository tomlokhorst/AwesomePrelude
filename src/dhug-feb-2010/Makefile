SRC = presentation.tex ArithExpr.tex Expr.tex BoolLike.tex RealExpr.tex Value.tex Conclusion.tex code.fmt

.PHONY : clean $(SRC:%.tex=ghci-%)

ghci-% :
	ghci -pgmL lhs2tex -optL --pre $*.lhs

presentation.pdf : $(SRC)
	latexmk -pdf -g $<
	open presentation.pdf

%.tex : %.lhs
	lhs2TeX -o $@ $<

preview :
	latexmk -pdf -pvc presentation

clean :
	latexmk -C
	for file in $(SRC:%.tex=%.lhs); do if [ -e $$file ]; then rm -f $${file%lhs}tex; fi; done
	rm -f presentation.ptb
	rm -f presentation.nav
	rm -f presentation.snm

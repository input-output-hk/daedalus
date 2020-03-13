all:					\
	launch.png			\
	update-system-old.png		\
	component-diagram.png		\
	update-system-byron.html	\
	windows-clean-shutdown.html     \
	$(EMPTY)

%.png: %.msc
	mscgen -T png -i $< -o $@

%.png: %.svg
	rsvg-convert -o $@ $<

%.html: %.md
	pandoc -f markdown -t html --standalone -o $@ $<

EMPTY :=

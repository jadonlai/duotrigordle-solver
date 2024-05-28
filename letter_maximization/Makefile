CXX ?= g++
CXXARGS ?= -O2 -std=c++20

letter_maximization: letter_maximization.cpp
	$(CXX) $(CXXARGS) -o $@ $<

.PHONY: clean

clean:
	rm -f letter_maximization

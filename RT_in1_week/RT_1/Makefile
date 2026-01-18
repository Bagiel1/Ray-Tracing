CXX = g++
CXXFLAGS = -O3 -Wall
TARGET = main
SOURCE = main.cpp
OUTPUT = image.ppm

HEADERS = $(wildcard *.h)

all: view

$(TARGET): $(SOURCE) $(HEADERS)
	@echo "Compilando código porque algo mudou..."
	$(CXX) $(CXXFLAGS) $(SOURCE) -o $(TARGET)

$(OUTPUT): $(TARGET)
	@echo "Renderizando imagem..."
	./$(TARGET) > $(OUTPUT)

view: $(OUTPUT)
	@echo "Abrindo imagem..."
	feh $(OUTPUT)

clean:
	rm -f $(TARGET) $(OUTPUT)

.PHONY: all view clean
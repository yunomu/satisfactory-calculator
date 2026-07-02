.PHONY: build clean release test

ELM_DIR			= src
SRCS			= $(shell find $(ELM_DIR) -regex '.*\.elm')
PUBLISH_DIR		= public
RESOURCE_DIR	= static
FILES			= index.html
OUTPUT			= static/main.js

RESOURCES		= $(addprefix $(RESOURCE_DIR)/,$(FILES))
TMPDIR			= .tmp
OPTIMIZED		= $(TMPDIR)/optimized.js
UGLIFIED		= $(TMPDIR)/uglified.js
RESOURCES_PUB	= $(addprefix $(PUBLISH_DIR)/,$(FILES))
TARGET			= $(PUBLISH_DIR)/main.js


build: $(OUTPUT)

$(OUTPUT): $(SRCS) elm.json
	elm make $(ELM_DIR)/Main.elm --output=$@

test:
	npx elm-test

clean:
	rm -rf $(OUTPUT) $(PUBLISH_DIR) $(TMPDIR)

$(PUBLISH_DIR):
	mkdir $@

$(TMPDIR):
	mkdir $@

$(OPTIMIZED): $(TMPDIR)
	elm make $(ELM_DIR)/Main.elm --output=$@ --optimize

$(UGLIFIED): $(OPTIMIZED)
	uglifyjs $^ --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output $@

$(TARGET): $(PUBLISH_DIR) $(UGLIFIED)
	cp $(UGLIFIED) $@

$(PUBLISH_DIR)/%: $(RESOURCE_DIR)/%
	cp $< $@

release: $(TARGET) $(RESOURCES_PUB)
	rm -rf $(TMPDIR)

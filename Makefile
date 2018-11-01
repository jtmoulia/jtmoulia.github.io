BUILD_DIR = dist
SRC_DIR = src
ORG_PROJECT = hlog

.PHONY: all build clean

all: build

build: $(BUILD_DIR) $(BUILD_DIR)/css/mystyles.css

$(BUILD_DIR):
	emacsclient --eval '(org-publish "$(ORG_PROJECT)" t)'

$(BUILD_DIR)/css/mystyles.css: $(SRC_DIR)/mystyles.scss
  # TODO prod!
	yarn run build

clean:
	rm -r $(BUILD_DIR)

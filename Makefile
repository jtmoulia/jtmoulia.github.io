BUILD_DIR = build
ORG_PROJECT = hlog

.PHONY: all clean

all: $(BUILD_DIR)

$(BUILD_DIR):
	emacsclient --eval '(org-publish "$(ORG_PROJECT)")'

clean:
	rm -r $(BUILD_DIR)

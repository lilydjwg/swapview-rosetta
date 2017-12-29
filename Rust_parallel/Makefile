.PHONY: all clean

target = $(shell \
  cd .swapview; \
  if type jshon >/dev/null 2>&1; then \
    cargo metadata --format-version=1 | jshon -e target_directory -u; \
  elif type jq >/dev/null 2>&1; then \
    cargo metadata --format-version=1 | jq -r .target_directory; \
  else \
    echo .swapview/target; \
  fi; \
)

%: %.rs
	cd .swapview && cargo build --release
	strip $(target)/release/swapview
	ln -srf $(target)/release/swapview

all: swapview

clean:
	-rm -rf $(target)/release/swapview

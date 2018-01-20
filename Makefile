
-include config.mk

MAGIT_DIR ?= /dev/null
MAGIT_POPUP_DIR ?= /dev/null
GHUB_DIR ?= /dev/null
DASH_DIR ?= /dev/null
WITH_EDITOR_DIR ?= /dev/null

LOAD_PATH = -L $(DASH_DIR) -L $(WITH_EDITOR_DIR) -L $(GHUB_DIR) \
	    -L $(MAGIT_POPUP_DIR) -L $(MAGIT_DIR)
BATCH = emacs -Q --batch $(LOAD_PATH)

all: magit-annex.elc magit-annex-autoloads.el

.PHONY: test
test: magit-annex.elc
	@$(BATCH) -L . -l magit-annex-tests.el \
	--eval "(ert-run-tests-batch-and-exit '(not (tag interactive)))"

.PHONY: clean
clean:
	$(RM) magit-annex.elc magit-annex-autoloads.el

%.elc: %.el
	@$(BATCH) -f batch-byte-compile $<

%-autoloads.el: %.el
	@$(BATCH) --eval \
	"(let (make-backup-files) \
	  (update-file-autoloads \"$(CURDIR)/$<\" t \"$(CURDIR)/$@\"))"

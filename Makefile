
-include config.mk

MAGIT_DIR ?= /dev/null
TRANSIENT_DIR ?= /dev/null
COMPAT_DIR ?= /dev/null
LLAMA_DIR ?= /dev/null
WITH_EDITOR_DIR ?= /dev/null
EMACSBIN ?= emacs

LOAD_PATH = -L $(COMPAT_DIR) -L $(LLAMA_DIR) -L $(WITH_EDITOR_DIR) \
	    -L $(TRANSIENT_DIR) -L $(MAGIT_DIR)
BATCH = $(EMACSBIN) -Q --batch $(LOAD_PATH)

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
	"(let ((make-backup-files nil)) \
	   (if (fboundp 'loaddefs-generate) \
	       (loaddefs-generate default-directory \"$@\" \
				  (list \"magit-annex-tests.el\")) \
	     (update-file-autoloads \"$(CURDIR)/$<\" t \"$(CURDIR)/$@\")))"

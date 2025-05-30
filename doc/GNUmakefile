MOBILE_TOP = ..


DOC_BASE_NAME := Ceylan-Mobile-Layer-technical-manual-english


OVERALL_DOCUMENT_SOURCE := $(DOC_BASE_NAME).rst


CURRENT_MOBILE_HTML := $(DOC_BASE_NAME).html

HTML_TARGET := $(CURRENT_MOBILE_HTML)


CURRENT_MOBILE_PDF := $(DOC_BASE_NAME).pdf

PDF_TARGET := $(CURRENT_MOBILE_PDF)


DOC_GENERATED_FILES := $(HTML_TARGET) $(PDF_TARGET)



PROJECT_CSS := pygments-default.css,mobile.css

PROJECT_ICON := mobile-icon.png


# Read from the user's environment:
GITHUB_PAGES_BASE := $(shell basename "$(MOBILE_WEB_MIRROR_BRANCH)")



.PHONY: all doc html pdf create-links clone-mirror                     \
		export-doc export-doc-to-official export-doc-to-mirror         \
		clean-links-local info info-web



# Default do-nothing target:
all:
	@echo "  Run 'make doc' to generate the documentation of the 'Mobile' layer."


doc: html pdf create-api-doc


html: local-html

pdf: local-pdf


# These links do not need to be in version control:
create-links:
	@/bin/ln -sf $(CURRENT_MOBILE_HTML) mobile.html
	@/bin/ln -sf mobile.html index.html
	@/bin/ln -sf $(CURRENT_MOBILE_PDF) mobile.pdf



# Creates a separate repository for the GitHub pages branch:
clone-mirror:
	@echo "  Creating a separate repository for website in $(GITHUB_PAGES_BASE)"
	@cd ../../ && git clone https://github.com/Olivier-Boudeville/Ceylan-Mobile.git $(GITHUB_PAGES_BASE) && cd $(GITHUB_PAGES_BASE) && git checkout -b gh-pages
	@echo "Please remove all initial content in that selected 'gh-pages' branch."


export-doc: clean-doc doc export-doc-to-official export-doc-to-mirror


# Drawback: scp will not preserve symlinks, it will duplicate files instead.
#
# rsync would not, but requires a different source layout (copies a full tree,
# not a set of files), so currently not used:
# rsync -avze "ssh $(SP)" $(SOME_WEB_TREE) $(WEB_SRV):$(WEB_ROOT)/Mobile/
#
export-doc-to-official: clean-doc create-links doc
	@echo "   Exporting Mobile documentation to official website ($(WEB_SRV))"

toto:
	@/bin/scp $(SP) -r $(WEB_CONTENT) $(WEB_SRV):$(WEB_ROOT)/Mobile/


# H option for 'cp', so that any local MathJax is copied (not symlinked to a
# local directory) in target branch:
#
export-doc-to-mirror: clean-doc create-links doc
	@echo "   Exporting Mobile documentation to mirror website in $(MOBILE_WEB_MIRROR_BRANCH)"
	@/bin/cp -rfH $(WEB_CONTENT) $(MOBILE_WEB_MIRROR_BRANCH) && cd $(MOBILE_WEB_MIRROR_BRANCH) && git add . && git merge -s ours && git commit -m "Mobile doc updated." && git push && git pull --ff-only


clean-links: clean-links-local

clean-links-local:
	-@/bin/rm -f mobile.html mobile.pdf



info: info-doc info-web


info-web:
	@echo "HTML_TARGET = $(HTML_TARGET)"
	@echo "SP = $(SP)"
	@echo "WEB_CONTENT = $(WEB_CONTENT)"
	@echo "WEB_SRV = $(WEB_SRV)"
	@echo "WEB_ROOT = $(WEB_ROOT)"
	@echo "MOBILE_WEB_MIRROR_BRANCH = $(MOBILE_WEB_MIRROR_BRANCH)"
	@echo "GITHUB_PAGES_BASE = $(GITHUB_PAGES_BASE)"


DOCUTILS_TOP = .

# For MYRIAD_TOP:
include $(MOBILE_TOP)/GNUmakesettings.inc

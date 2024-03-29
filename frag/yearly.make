ASSIGN_HWS = $(BIG_HW:%=%/assignment.json)
MKASGN = $(LEVEL)/hsExprTest/frag/support/mkassignment
HSDEPS = $(wildcard $(LEVEL)/hsExprTest/frag/support/*.hs) $(wildcard $(LEVEL)/hsExprTest/frag/common/*.hs)

assignments : $(ASSIGN_HWS)

%/assignment.json : %/Test.hs $(MKASGN) $(HSDEPS) %/assignment.part.json
	$(MKASGN) $< $(SEM_END) $(dir $@)

import : $(ASSIGN_HWS)
	$(FRAG) import

$(BIG_HW:%=assignment.%) : assignment.% : %/assignment.json

$(BIG_HW:%=import.%) : import.% : %/assignment.json
	(cd $(@:import.%=%); $(FRAG) import)

.PHONY: import assignments

# vim: ft=make

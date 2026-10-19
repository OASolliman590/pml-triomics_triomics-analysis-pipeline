# Tri-omics progress

**Goal:** Build a reproducible pipeline for quantified proteomics, metabolomics
and lipidomics while keeping the fasting, diabetic-treatment and
organophosphate studies scientifically separate.

**Works now:** Stage B’s independent, paired, repeated-visit, DEqMS/fallback,
multiplicity, clinical and export lanes run together on synthetic data.
All nine required Stage B packets and G4 are accepted. C-01 is accepted.
Stage C can combine keyed blocks with concatenation PCA, and the C-02
supervised route can run leakage-safe nested PLS-DA on the synthetic fixture.

**Changed:** C-02’s eight review findings are closed in the local code.
The C-02 suite passed 165 checks. The synthetic stage C command exited 0 and
predicted all 12 fixture samples with nested concatenation PLS-DA. A two-sample
inner fold that asked for more components than its rank failed explicitly.
C-02 is accepted as synthetic software evidence. The M5 review found no
blocking defect, so DIABLO, block.spls, and MOFA may start. The DIABLO adapter
is now in the local branch as software code only. It is not accepted, and the
command-line route still refuses `mixomics_diablo`. Later Stage C and all
Stage D packets stay closed.

**Not established:** Accepted evidence remains synthetic software evidence, not
biological validation. A separate local inventory reviewed the three private
study workbooks, but the accepted pipeline has not fitted them. B-T04’s ordinary
23/400 diagnostic remains failed; only its frozen production route passed.

**Next:** C-03 needs an independent review before it can be accepted. C-04 is
the other open block.spls packet. C-05 waits on a MOFA2 and mofapy2
environment. C-08 and every D packet still wait on their own dependencies.

**Needs Omar:** Decide when the clean, reviewed local history may be published.
No private study data may enter GitHub or Notion.

**Sync:** GitHub read-back confirms public `main` still contains only commit
`c8f0eec`; the hardened branch and 227 later local commits are unpublished. A
Notion development report and links were created and verified by read-back.

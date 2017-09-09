
rule dataset:
    input: "data/features_clip_v2.Rda", "functions.R", script = "dataset.R"
    output: expand("data/{set}.Rda", set = [ "training", "test" ] )
    shell: "Rscript {input.script}"

rule correctFeatures:
    input: data = "data/{set}.Rda", script = "correct_features.R"
    output: "data/{set}_corrected.Rda"
    shell: "Rscript {input.script} {input.data} {wildcards.set} {output}"

rule topTables:
    input: "data/training_corrected.Rda", script = "top_tables.R"
    output: "data/topTables.Rda"
    shell: "Rscript {input.script}"

rule exportTopTables:
    input: data=rules.topTables.output, script = "export_top_tables.R"
    output: "results/top_tables/top_tables.xlsx"
    shell: '''
        d=`dirname {output}`
        mkdir -p $d
        Rscript {input.script} {input.data} $d
        ssconvert --merge-to {output} $d/*.txt
        '''

rule pca:
    input: data = "data/training_corrected.Rda", f = "functions.R", 
        script = "pca.R"
    output: "results/plots/pca.pdf"
    shell: '''
        d=`dirname {output}`
        mkdir -p $d
        Rscript {input.script} {input.data} {output} professional.diagnosis
        '''

rule reduceFeatures:
    input: "data/training_corrected.Rda", "params/maxD", 
        script = "reduce_features.R"
    output: plots = "results/plots/reduce_features.pdf", 
        data = "data/training_reduced.Rda"
    shell: '''
        d=`dirname {output.plots}`
        mkdir -p $d
        Rscript {input.script}
        '''

rule splsda:
    input: "data/training_corrected.Rda", script = "glm_spls_model.R"
    output: "data/glm_spls_professional.diagnosis_model.Rda"
    shell: "Rscript {input.script} professional.diagnosis"

rule gaRun:
    input: rules.reduceFeatures.output, rules.topTables.output, 
        script = "glm_ga_run.R"
    output: temp("data/glm_ga_run_{r}.Rda")
    shell: "Rscript {input.script} professional.diagnosis {output}"

rule gaModel:
    input: runs = expand("data/glm_ga_run_{r}.Rda", r = [ "%02d" % i for i in range(4) ]),
        dataset = "data/training_corrected.Rda", script = "glm_ga_model.R"
    output: "data/glm_ga_professional.diagnosis_model.Rda"
    shell: "Rscript {input.script} {output} professional.diagnosis {input.runs}"

rule exhaustiveModel:
    input: rules.reduceFeatures.output, rules.topTables.output,
        script = "glmulti_exhaustive.R"
    output: "data/glm_exhaustive.Rda"
    shell: "Rscript {input.script}"

rule fullModel:
    input: "data/training_corrected.Rda", script = "full_model.R"
    output: "data/glm_full.Rda", "data/glm_stepped.Rda"
    shell: "Rscript {input.script}"

rule ROC:
    input: rules.gaModel.output, rules.splsda.output, rules.fullModel.output,
        rules.exhaustiveModel.output, "data/test_corrected.Rda",
        script = "auroc.R"
    output: "results/plots/ROCs.pdf"
    shell: '''
        d=`dirname {output}`
        mkdir -p $d
        Rscript {input.script}
        '''

rule all:
    input: rules.ROC.output, rules.pca.output, rules.exportTopTables.output

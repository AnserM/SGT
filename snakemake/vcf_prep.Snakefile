
import os
import sys

def extract_names():
    names = []
    directory = "/project/bilyeu_soybean_genomic_merge/anserm/datasets/Soy2939/GATK_GenotypeGVCFs_gz/"
    for filename in os.listdir(directory):
        if filename.endswith(".vcf.gz"):
            # Remove and ".vcf.gz"
            names.append(filename[:-7])
    return names

names = extract_names()

rule all:
    input:
        expand("./dup_rm_renamed/{name}_nohets_dr_renamed.vcf.gz", name=names)

rule hets:
    input:
        "/project/bilyeu_soybean_genomic_merge/anserm/datasets/Soy2939/GATK_GenotypeGVCFs_gz/{name}.vcf.gz"
    output:
        "./hets/{name}_nohets.vcf.gz"
    shell:
        """
        bcftools view -Q 0.99999 -a -c1 {input} -Oz -o {output}
        bcftools index -t {output}
        """

rule remove_dup:
    input:
        "./hets/{name}_nohets.vcf.gz"
    output:
        "./dup_rm/{name}_nohets_dr.vcf.gz"
    shell:
        """
	bcftools view -s ^CRR107303,CRR106989,CRR106996,CRR106995,CRR108731,CRR108771,CRR108736,CRR107602,CRR107014,CRR108615,CRR108769,CRR108755,CRR107022,CRR107433,CRR106968,CRR108777,CRR107258,CRR107124,CRR108739,CRR106893,CRR107488,CRR107423 -o {output} {input}
        """


rule rename:
    input:
        "./dup_rm/{name}_nohets_dr.vcf.gz"
    output:
        "./dup_rm_renamed/{name}_nohets_dr_renamed.vcf.gz"
    shell:
        """
        bcftools reheader -s samples.txt -o {output} {input}
	bcftools index -t {output}
        """

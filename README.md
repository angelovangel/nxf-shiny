# nxf-shiny
Dynamically generate a GUI to collect parameters and run any nextflow pipeline.

### Core idea
- The parameters of a nextflow pipeline are described in a json file, which is used to dynamically generate the 
Shiny inputs. This file is placed unde `pipelines`
- The collected user inputs are passed to nextflow runtime as `-params-file pipeline.json`.
- The pipeline is executed in a tmux process that is monitored by Shiny, the results are diplayed in a table for download

### JSON files
The pipeline parameters JSON describes the Shiny inputs needed and correspond to the pipeline parameters.
Example for file and select input:
```json
[
{
    "type": "fileInput",
    "inputId": "ref",
    "label": "Upload reference",
    "required": true,
    "multiple": false,
    "accept": [".fa", ".fasta", ".dna", ".gbk", ".genbank", ".embl"],
    "placeholder": "reference"
  },
  {
    "type": "selectInput",
    "inputId": "format",
    "label": "Reference format",
    "required": false,
    "multiple": false,
    "choices": {
      "Fasta": "fasta",
      "Genbank": "genbank",
      "EMBL": "embl",
      "Snapgene": "snapgene"
    }
  }
]
```
The keys correspond to the Shiny input objects.
The `inputId` corresponds to the extflow parameter name, and the value provided by the user is passed to nextflow.
For the above example, this is given to nextflow as:
```
nextflow run pipeline --fastq path/to/file --format fasta ...
```
> Some JSON keys are not used in Shiny input generation, but for conditional display or treat them as required. These are:
> - `panel_condition`
> - `required`

### Add a new pipeline
The steps to add a new nextflow pipeline for use with this Shiny app:
- write a JSON file that defines the Shiny inputs 
- place the file in the `pipelines` folder
- add the pipeline in the `config.json` that will make it available in the select pipeline dropdown:

```json
[
  {
    "type": "selectInput",
    "inputId": "pipelines",
    "label": "Select pipeline to run",
    "multiple": false,
    
    "choices": {
      "nxf-minimapper": "nxf-minimapper.json",
      "nxf-tgs": "nxf-tgs.json"
    }
  }
]
```

>Note about profiles   
The `nextflow -profile` can be used by defining the profiles in the input json, like a normal parameter with `"inputId": "profile"` 
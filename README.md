# nxf-shiny
Dynamically generate a GUI to collect parameters and run any nextflow pipeline.

### Core idea
- The parameters of a nextflow pipeline are described in a json file, which is used to dynamically generate the 
Shiny inputs. This file is placed unde `pipelines`
- The collected user inputs are passed to nextflow runtime as `-params-file pipeline.json`.
- The pipeline is executed in a tmux process that is monitored by Shiny, the results are diplayed in a table for download

### JSON files
The pipeline parameters JSON describes the Shiny inputs needed and correspond to the pipeline parameters.
Example for file input:
```json
[
{
    "type": "fileInput",// <--- this is the type of Shiny input to generate
    "inputId": "fastq", // <--- this is the name of the nextflow pipeline param
    "label": "Fastq",   // <--- Shiny input param
    "required": true,   // <--- Shiny input param
    "multiple": true,   // <--- Shiny input param
    "accept": [".fasta", ".gz", "fq"], // <--- Shiny input param
    "placeholder": "fastq file(s)",   // <--- Shiny input param
    "help": "help text" // <--- not a Shiny input param
  }, 
  {
    "type": "fileInput",
    "inputId": "ref",
    "label": "Upload reference",
    "required": true,
    "multiple": false,
    "accept": [".fa", ".fasta", ".dna", ".gbk", ".genbank", ".embl"],
    "placeholder": "reference"
  }
]
```
The keys correspond to the parameters of the Shiny input objects.
The `inputId` corresponds to the Nextflow parameter name, and the value provided by the user is passed to nextflow.
For the above example, this is given to nextflow as:
```
nextflow run pipeline --fastq path/to/file --format fasta ...
```
### Supported Shiny input types
All Shiny input types can be used - https://shiny.posit.co/r/getstarted/build-an-app/reactive-flow/ui-inputs.html

> Some JSON keys are not used in Shiny input generation, but for conditional display or help text. These are:
> - `panel_condition`
> - `required`
> - `help`

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
The `nextflow -profile` can be used by defining the profiles in the input json, like a normal parameter with `"inputId": "profile"`. Currently, pipeline parameters defined with `-profile` is not working.
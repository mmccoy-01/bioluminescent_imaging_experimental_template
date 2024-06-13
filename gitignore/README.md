Because Github classifies .Rmd as prose, I duplicated the {{experimental_name}} code and changed the file extension from .Rmd to .R

That way, on my repository, it shows the language as being R.

The way around this would to publish each chunk of code in the .Rmd file as a standalone .R file. However, to avoid possible code degradation, I've opted to have all code within one .Rmd file so as to avoid newer code being affected by older standalone .R files. 

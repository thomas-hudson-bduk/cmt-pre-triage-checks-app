Created by Thomas Huson (S&DI) on 27/03/23.

This repository creates an R shiny application, which is used by the BDUK Case Management Team to pre-triage supplier data.

This code contains no senstive information (and hence why storing it publically is possible, as well as being transparent). The code performs checks on (locally) uploaded datasets, such as checking for duplicates and when a technology is full-fibre checking that it's maximum download speed is at least 1000 MBps.

This code is designed to be run locally on RStudio using the runGitHub() command. The ease in running a (public) shiny app in this way is why it is being. It allows the sensitive data to be kept locally (rather than uploaded to a server) whilst also allowing remote updates to the code (which would be harder for an executable/ locally saved file).

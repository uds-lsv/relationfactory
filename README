RelationFactory is a relation extraction and knowledge-base population system.
It was the top-ranked system in TAC KBP 2013 English Slot-filling (http://www.nist.gov/tac/2013/KBP/index.html).
If you want to use RelationFactory in a TAC benchmark, please contact the authors (see LICENSE for details).
RelationFactory uses SVMLight (http://svmlight.joachims.org/) for classification, so you must agree to the 
License of SVMLight, especially to it being restricted to scientific use only.

QUICK START
===========


0. Prerequisites

Make sure the following software is installed:

ghc, version >= 7.4.1
cabal, version >= 1.14.0
java / JDK, version >= 6 (the Oracle one)
unix tools, including wget


1. Download models

If you want to use pre-trained models, download them from our server:

wget https://www.lsv.uni-saarland.de/fileadmin/data/relationfactory_models.tar.gz
tar xzf relationfactory_models.tar.gz


2. Set paths

E.g. by putting the following lines in your ~/.bashrc :

# relationfactory clone
export TAC_ROOT=/path/to/relationfactory
# pre-trained models
export TAC_MODELS=/path/to/relationfactory_models

The TAC_ROOT variable has to be set. The TAC_MODELS variable is optional. 
If it is not set, the models have to be specified in the config file.


3. Compile system

$TAC_ROOT/bin/generate_system.sh


4. Index corpus

See the corresponding README in $TAC_ROOT/indexing


5. Configure run

The settings can be taken from $TAC_ROOT/config/system2013.config .
Make sure to adapt it to your models and index locations.
Also point to the TAC queries file for which you want to get results, and
specify a rundir where files for that run are put.


6. Run

$TAC_ROOT/bin/run.sh your_system.config


7. Check response

check the output file, /your/rundir/response_fast_pp13. It should contain 
for each query some mixture of NIL answers and other answers, many of which 
score by 1.0, others with lower score.

Evaluate your run using the official TAC scorer.
Note that due to refactoring, slightly different answers are returned than in TAC 2013.
The 'exact' evaluation, that is dependent on document id's and offsets to be included in the answer pool,
is very sensitive to that.
Use 'anydoc' evaluation mode to obtain more robust scores.

8. How to change the pipeline

Change $TAC_ROOT/bin/makefile and insert a rule describing your new target.

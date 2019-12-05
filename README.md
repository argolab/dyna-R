# Dyna-R

The Dyna programming language built on R-exprs

## install instructions
```
git clone git@github.com:matthewfl/dyna-R.git
cd dyna-R
python -m venv /tmp/dyna
source /tmp/dyna/bin/activate
pip install -r requirements.txt
python setup.py develop

# run tests
pytest

# start repl
dyna   OR   python -m dyna.repl
```

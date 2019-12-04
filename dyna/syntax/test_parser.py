
from dyna.syntax.normalizer import add_rules, user_query

def test_user_query():

    add_rules("""
    fff(X) = 1.
    """)

    user_query('fff(1)')

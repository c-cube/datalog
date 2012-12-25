% product shipping example

ship_to(ProdName, City) :-
    has_ordered(CustNo, ProdNo),
    customer_city(CustNo, City),
    product_name(ProdNo, ProdName).

customer_city(1, london).
customer_city(2, paris).
customer_city(3, 'San Francisco').
customer_city(4, munich).
customer_city(5, seoul).

has_ordered(1, 1).
has_ordered(2, 2).
has_ordered(3, 3).
has_ordered(4, 4).
has_ordered(5, 5).

product_name(1, tea).
product_name(2, bread).
product_name(3, flowers).
product_name(4, sausage).
product_name(5, horse).

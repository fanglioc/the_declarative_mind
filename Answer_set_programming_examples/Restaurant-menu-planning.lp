% Menu items and categories
dish(pasta_carbonara). dish(grilled_salmon).
dish(caesar_salad). dish(veggie_curry).
dish(chocolate_cake). dish(fruit_salad).

category(appetizer). category(main). category(dessert).

% Properties of dishes
vegetarian(veggie_curry). vegetarian(caesar_salad).
contains_dairy(pasta_carbonara). contains_dairy(chocolate_cake).
gluten_free(grilled_salmon). gluten_free(fruit_salad).

% Assign dishes to categories
dish_category(caesar_salad, appetizer).
dish_category(pasta_carbonara, main).
dish_category(grilled_salmon, main).
dish_category(veggie_curry, main).
dish_category(chocolate_cake, dessert).
dish_category(fruit_salad, dessert).

% Select dishes for menu
{ selected(D) } :- dish(D).

% Menu constraints
:- category(C), not 1 { selected(D) : dish_category(D,C) }.
:- not 2 { selected(D) : vegetarian(D) }.
:- not 2 { selected(D) : gluten_free(D) }.

#show selected/1.
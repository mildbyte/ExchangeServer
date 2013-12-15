ExchangeServer
==============

A stock exchange "server" that can receive orders for various fake commodities from clients, match and execute them.

Why, Lisa, why?!
----------------

I am currently learning Scala and so wanted to try out writing something in it, also, I wanted to build an economy simulation as described in "Emergent Economies for Role Playing Games" (http://larc.unt.edu/techreports/LARC-2010-03.pdf). The code so far is very imperative and there are probably many Javaisms there. The passwords are stored in plaintext, but there is no persistence anyway and there are two users (named test and test2), so I guess you could use it in a live environment safely (tee-hee).

Protocol
--------

Dead simple. A user logs in with LOGIN <name> <password>, can buy or sell stocks with {BUY, SELL} <name> <amount> <price> and will be replied with OK <order id> or FAIL, can cancel their order with CANCEL <order id>, can view their orders with ORDERS and the balance with BALANCE. Oh, and LOGOUT to log out.

Execution
---------

(see page 9 of the referenced paper for the full pseudocode that inspired this.) The order book for every commodity is matched every second. All bids are sorted in the descending price order and all asks are sorted in the ascending price order. We can match a bid and an ask if the bid price is greater than or equal to the ask price. The two matching offers are executed and, if they are executed fully (the amounts being sold and bought are equal), removed from the book, otherwise, one order will be fully filled and the other one will be partially filled and so will stay in the book. The server will also notify the client that their order has been executed. This matching will continue until there are no matching orders in the book.
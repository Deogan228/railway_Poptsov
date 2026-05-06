## Controls
- Enter menu item number and press Enter.
- "0" to go back / exit.

## Variant 13. Railway Ticket Office

A ticket office that sells tickets, accounts for baggage and available seats.

### Reader
From environment: route tariffs, baggage surcharge, seat rule, refund penalty.

### Writer
Logging: route selection, price calculation, seat assignment, refund.

### State
State: seats per train, sold tickets, revenue.
Transitions: bookTicket, cancelTicket, addTrain, nextDay.

### IO
Scenario: choose train, choose seat, buy or return a ticket.

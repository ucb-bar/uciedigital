module ClockMux2 (
    input clocksIn_0,
    input clocksIn_1,
    input sel,
    output clockOut
);

    // REPLACE ME WITH A CLOCK CELL IF DESIRED

    // XXX be careful with this! You can get really nasty short edges if you
    // don't switch carefully
    assign clockOut = sel ? clocksIn_1 : clocksIn_0;

endmodule
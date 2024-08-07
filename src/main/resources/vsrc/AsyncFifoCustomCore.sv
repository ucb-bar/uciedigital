// Code your design here
module AsyncFifoCustomCore #(
    parameter DEPTH = 16,
    parameter WIDTH = 8
)(
    input rst,

    input clk_w,
    input valid_w,
    output ready_w,
    input [WIDTH-1:0] data_w,

    input clk_r,
    output valid_r,
    input ready_r,
    output [WIDTH-1:0] data_r
);
    localparam PTR_WIDTH = $clog2(DEPTH);


    wire [PTR_WIDTH:0] b_wptr;
    wire [PTR_WIDTH:0] b_rptr;
    wire [PTR_WIDTH:0] g_wptr;
    wire [PTR_WIDTH:0] g_rptr;
    wire [PTR_WIDTH:0] g_wptr_sync;
    wire [PTR_WIDTH:0] g_rptr_sync;
    wire full, empty;

    Sync2Flop #(PTR_WIDTH + 1) wptrSync  (.clk(clk_w), .rst(rst), .in(g_wptr), .out(g_wptr_sync));
    Sync2Flop #(PTR_WIDTH + 1) rptrSync  (.clk(clk_r), .rst(rst), .in(g_rptr), .out(g_rptr_sync));
    WptrHandler #(PTR_WIDTH) wptrHandler  (.clk(clk_w), .rst(rst), .en(valid_w), .g_rptr_sync(g_rptr_sync),
                                .g_wptr(g_wptr), .b_wptr(b_wptr), .full(full));
    RptrHandler #(PTR_WIDTH) rptrHandler  (.clk(clk_r), .rst(rst), .en(ready_r), .g_wptr_sync(g_wptr_sync),
                                .g_rptr(g_rptr), .b_rptr(b_rptr), .empty(empty));
    Fifo #(PTR_WIDTH, WIDTH) fifo (.rst(rst), 
                                    .clk_w(clk_w), .en_w(valid_w), .data_w(data_w), .b_wptr(b_wptr), .full(full),
                                    .clk_r(clk_r), .en_r(ready_r), .data_r(data_r), .b_rptr(b_rptr), .empty(empty));
    assign valid_r = ~empty;
    assign ready_w = ~full;
 
endmodule

module Sync2Flop #(
    parameter PTR_WIDTH = 8
)(
    input clk,
    input rst,
    input [PTR_WIDTH-1:0] in,
    output reg [PTR_WIDTH-1:0] out
);
    reg [PTR_WIDTH-1:0] mid;
    always_ff @(posedge clk, negedge rst) begin
        if (~rst) begin
            out <= '0;
            mid <= '0;
        end else begin
            out <= mid;
            mid <= in;
        end
    end
endmodule

module WptrHandler #(
    parameter PTR_WIDTH = 8
)(
    input clk,
    input rst,
    input en,
    input [PTR_WIDTH:0] g_rptr_sync,
    output reg [PTR_WIDTH:0] g_wptr, b_wptr,
    output reg full
);
    wire [PTR_WIDTH:0] g_wptr_next;
    wire [PTR_WIDTH:0] b_wptr_next;
    wire full_next;

    assign b_wptr_next = b_wptr + (en & ~full);
    assign g_wptr_next = b_wptr_next ^ (b_wptr_next >> 1);
    assign full_next = g_wptr_next == {~g_rptr_sync[PTR_WIDTH:PTR_WIDTH-1], g_rptr_sync[PTR_WIDTH-2:0]};

    always_ff @(posedge clk, negedge rst) begin
        if(~rst) begin
            g_wptr <= '0;
            b_wptr <= '0;
            full   <= '0;
            // g_wptr <= g_wptr_next;
            // b_wptr <= b_wptr_next;
            // full   <= full_next;
        end else begin
            g_wptr <= g_wptr_next;
            b_wptr <= b_wptr_next;
            full   <= full_next;
        end
    end

endmodule

module RptrHandler #(
    parameter PTR_WIDTH = 8
)(
    input clk,
    input rst,
    input en,
    input [PTR_WIDTH:0] g_wptr_sync,
    output reg [PTR_WIDTH:0] g_rptr, b_rptr,
    output reg empty
);
    wire [PTR_WIDTH:0] g_rptr_next;
    wire [PTR_WIDTH:0] b_rptr_next;
    wire empty_next;

    assign b_rptr_next = b_rptr + (en & ~empty);
    assign g_rptr_next = b_rptr_next ^ (b_rptr_next >> 1);
    assign empty_next = g_rptr_next == g_wptr_sync;

    always_ff @(posedge clk, negedge rst) begin
        if(~rst) begin
            g_rptr <= '0;
            b_rptr <= '0;
            empty   <= '1;
        end else begin
            g_rptr <= g_rptr_next;
            b_rptr <= b_rptr_next;
            empty   <= empty_next;
        end
    end

endmodule

module Fifo #(
    parameter PTR_WIDTH = 8,
    parameter WIDTH = 8
)(
    input rst,
    // Write
    input [WIDTH-1:0] data_w,
    input clk_w,
    input en_w,
    input [PTR_WIDTH:0] b_wptr,
    input full,
    // Read
    output reg [WIDTH-1:0] data_r,
    input clk_r,
    input en_r,
    input [PTR_WIDTH:0] b_rptr,
    input empty
);
    localparam ENTRIES = 2**PTR_WIDTH;
    integer i;
    reg [WIDTH-1:0] fifoBank [0:ENTRIES-1];
    always_ff @(posedge clk_w, negedge rst) begin
        if(~rst) begin
            for(i = 0; i < ENTRIES; i++) begin
                fifoBank[i] <= 'b0;
            end
        end else if (en_w & ~full) begin
            fifoBank[b_wptr[PTR_WIDTH-1:0]] <= data_w;
        end else begin
            for(i = 0; i < ENTRIES; i++) begin
                fifoBank[i] <= fifoBank[i];
            end
        end
    end

    // always_ff @(posedge clk_r) begin
    //     if(en_r & ~empty) begin
    //         data_r <= fifoBank[b_rptr[PTR_WIDTH-1:0]];
    //     end
    // end

    assign data_r = fifoBank[b_rptr[PTR_WIDTH-1:0]];
endmodule

module BinaryToGray #(
    parameter WIDTH = 8
)(
    input [WIDTH-1:0] in,
    output [WIDTH-1:0] out
);

genvar i;
assign out[WIDTH-1] = in[WIDTH - 1];
for(i = WIDTH - 2; i >= 0; i--) begin
    assign out[i] = in[i] ^ in[i + 1];
end

endmodule

module GrayToBinary #(
    parameter WIDTH = 8
)(
    input [WIDTH-1:0] in,
    output [WIDTH-1:0] out
);
genvar i;
assign out[WIDTH-1] = in[WIDTH - 1];
for(i = WIDTH - 2; i >= 0; i--) begin
    assign out[i] = ^(in >> i);
end
endmodule
`timescale 1ns/1ns

module main();

    // Toggle the reset line
    initial begin
        reset_reg = 1;
        reset_reg = #1 0;
        reset_reg = #2 1;
    end
    reg reset_reg;
    wire reset = reset_reg;
    
    // 1GHz virtual clock
    reg theClock = 0;
    assign clk = theClock;
    always begin
        #50;
        theClock = !theClock;
    end

    wire halt;
    wire output_valid;
    wire [63:0] output_data;

    CPU_topEntity evaluator(clk, reset, halt, output_valid, output_data);
    
    always@(posedge clk) begin
        if (output_valid == 1) begin
            $display("0x%h", output_data);
        end else begin
            $display(".");
        end
    end

    always@(posedge clk) begin
        if (halt == 1) $finish;
    end

endmodule
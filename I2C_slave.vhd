----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 14.05.2023 18:05:33
-- Design Name: 
-- Module Name: i2c_slave - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.std_logic_unsigned.all;

ENTITY i2c_slave IS
  GENERIC(
    input_clk : INTEGER := 50_000_000; --input clock speed from user logic in Hz
    bus_clk   : INTEGER := 400_000);   --speed the i2c bus (scl) will run at in Hz
  PORT(
    clk       : IN     STD_LOGIC;                    --system clock
    reset_n   : IN     STD_LOGIC;                    --active low reset
    ena       : IN     STD_LOGIC;                    --enable transaction
    rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
    data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); --data to write 
    data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from master
    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC);                   --serial clock output of i2c bus
END i2c_slave;

ARCHITECTURE logic OF i2c_slave IS
  CONSTANT divider  :  INTEGER := (input_clk/bus_clk)/4; --number of clocks in 1/4 cycle of scl
  TYPE machine IS(ready, read_addr, read, write, ack, ack2, ack3); --needed states
  SIGNAL state         : machine;                        --state machine
  SIGNAL data_clk      : STD_LOGIC;                      --data clock for sda
  SIGNAL data_clk_prev : STD_LOGIC;                      --data clock during previous system clock
  SIGNAL scl_clk       : STD_LOGIC;                      --internal scl
  SIGNAL scl_ena       : STD_LOGIC := '0';               --enables internal scl
  SIGNAL sda_int       : STD_LOGIC := '1';               --internal sda
  SIGNAL addr_rw       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --address and read/write
  SIGNAL data_tx       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --data to write to master
  SIGNAL data_rx       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --data received from master
  SIGNAL addr_rx       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --address send by master
  SIGNAL bit_cnt       : INTEGER RANGE 0 TO 7 := 7;      --tracks bit number in transaction
BEGIN


  PROCESS(clk, reset_n)
    VARIABLE count  :  INTEGER RANGE 0 TO divider*4;  
  BEGIN
    IF(reset_n = '0') THEN               
      count := 0;
    ELSIF(clk'EVENT AND clk = '1') THEN
      data_clk_prev <= data_clk;          
      IF(count = divider*4) THEN       
        count := 0;                      
      ELSE
        count := count + 1;               
      END IF;
      CASE count IS
        WHEN 0 TO divider-1 =>            --first 1/4 cycle of clocking
          scl_clk <= '0';
          data_clk <= '0';
        WHEN divider TO divider*2-1 =>    --second 1/4 cycle of clocking
          scl_clk <= '0';
          data_clk <= '1';
        WHEN divider*2 TO divider*3-1 =>  --third 1/4 cycle of clocking
          scl_clk <= '1';                 
          data_clk <= '1';
        WHEN OTHERS =>                    --last 1/4 cycle of clocking
          scl_clk <= '1';
          data_clk <= '0';
      END CASE;
    END IF;
  END PROCESS;

 
  PROCESS(clk, reset_n)
  BEGIN
    IF(reset_n = '0') THEN               
      state <= ready;                                  
      bit_cnt <= 7;                       
      data_rd <= "00000000";              
    ELSIF(clk'EVENT AND clk = '1') THEN
      IF(data_clk = '1' AND data_clk_prev = '0') THEN  
        CASE state IS
          WHEN ready =>                      
            IF(ena = '1' AND sda = data_clk_prev) THEN
              data_tx <= data_wr;                  
              state <= read_addr;              
            ELSE                           
              state <= ready;               
            END IF;
            sda_int <= 'Z';
            
          WHEN read_addr =>   -- read address and rw  
            IF(bit_cnt = 0) THEN
              sda_int <= '0';
              bit_cnt <= 7;
              state <= ack;
            ELSE                
              bit_cnt <= bit_cnt - 1;
              state <= read_addr;
            END IF;
            
          WHEN ack =>                           -- acknowladge
              IF(rw = '1') THEN
                sda_int <= data_tx(bit_cnt);
                state <= write;
              ELSIF(rw = '0') THEN
                state <= read;
                sda_int <= 'Z';
              ELSE
                state <= ready;
                sda_int <= 'Z';
              END IF;
             
          WHEN write =>                        --write
            IF(bit_cnt = 0) THEN
              state <= ack2;
              sda_int <= 'Z'; 
              bit_cnt <= 7;         
            ELSE
              bit_cnt <= bit_cnt - 1;       
              sda_int <= data_tx(bit_cnt-1);
              state <= write;
            END IF;
            
          WHEN ack2 =>                         -- ack2
            sda_int <= 'Z';                                                 
            IF(rw = '1') THEN
                sda_int <= data_tx(bit_cnt);
                state <= write;
              ELSE 
                state <= ready;
                
              END IF; 
             
          WHEN read =>                          -- read
            IF(bit_cnt = 0) THEN
              bit_cnt <= 7;                
              data_rd <= data_rx;
              sda_int <= '0';
              state <= ack3;
            ELSE
            bit_cnt <= bit_cnt - 1;
            state <= read;
            END IF;
            
          WHEN ack3 =>                         --ack3
            sda_int <= 'Z';
            IF(rw = '0') THEN
              state <= read;
            ELSE
              state <= ready;
            END IF;               
        END CASE;
            
      ELSIF(data_clk = '0' AND data_clk_prev = '1') THEN  
        CASE state IS
          WHEN read_addr =>
            addr_rx(bit_cnt) <= sda;
          
          WHEN read =>                                
            data_rx(bit_cnt) <= sda;                
                                   
          WHEN OTHERS =>
            NULL;
        END CASE;
      END IF;
    END IF;
  END PROCESS;
    
        sda <= sda_int;
        scl <= 'Z';
END logic;

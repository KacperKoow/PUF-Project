----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 04.05.2023 12:20:25
-- Design Name: 
-- Module Name: I2C_master - Behavioral
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

ENTITY i2c_master IS
  GENERIC(
    input_clk : INTEGER := 50_000_000; --input clock speed from user logic in Hz
    bus_clk   : INTEGER := 400_000);   --speed the i2c bus (scl) will run at in Hz
  PORT(
    clk       : IN     STD_LOGIC;                    --system clock
    reset_n   : IN     STD_LOGIC;                    --active low reset
    ena       : IN     STD_LOGIC;                    --enable transaction
    addr      : IN     STD_LOGIC_VECTOR(6 DOWNTO 0); --address of slave
    rw        : IN     STD_LOGIC;                    --'0' is write, '1' is read
    data_wr   : IN     STD_LOGIC_VECTOR(7 DOWNTO 0); --data to write to slave
    busy      : OUT    STD_LOGIC;                    --indicates transaction in progress
    data_rd   : OUT    STD_LOGIC_VECTOR(7 DOWNTO 0); --data read from slave
    ack_error : INOUT  STD_LOGIC;                    --acknowledge from slave
    sda       : INOUT  STD_LOGIC;                    --serial data output of i2c bus
    scl       : INOUT  STD_LOGIC);                   --serial clock output of i2c bus
END i2c_master;

ARCHITECTURE logic OF i2c_master IS
  CONSTANT divider  :  INTEGER := (input_clk/bus_clk)/4; --number of clocks in 1/4 cycle of scl
  TYPE machine IS(ready, start, write_addres, slv_ack1, wr, rd, slv_ack2, mstr_ack, stop); --needed states
  SIGNAL state         : machine;                        --state machine
  SIGNAL data_clk      : STD_LOGIC;                      --data clock for sda
  SIGNAL data_clk_prev : STD_LOGIC;                      --data clock during previous system clock
  SIGNAL scl_clk       : STD_LOGIC;                      --internal scl
  SIGNAL scl_ena       : STD_LOGIC := '0';               --enables internal scl 
  SIGNAL sda_int       : STD_LOGIC := '1';               --internal sda
  SIGNAL addr_rw       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --address and read/write
  SIGNAL data_tx       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --data to write to slave
  SIGNAL data_rx       : STD_LOGIC_VECTOR(7 DOWNTO 0);   --data received from slave
  SIGNAL bit_cnt       : INTEGER RANGE 0 TO 7 := 7;      --tracks bit number in transaction
BEGIN

  
  PROCESS(clk, reset_n) --sets scl and sda_clk
    VARIABLE count  :  INTEGER RANGE 0 TO divider*4;  --divides clock to 4 cycles
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
      busy <= '1';                        
      scl_ena <= '0';                     
      sda_int <= 'Z';                    
      ack_error <= '0';                   
      bit_cnt <= 7;                       
      data_rd <= "00000000";              
    ELSIF(clk'EVENT AND clk = '1') THEN
      IF(data_clk = '1' AND data_clk_prev = '0') THEN  
        CASE state IS
          WHEN ready =>                      --idle state
            IF(ena = '1') THEN             
              busy <= '1';                  
              addr_rw <= addr & rw;        
              data_tx <= data_wr;           
              state <= start;              
            ELSE                           
              busy <= '0';                   
              state <= ready;               
            END IF;
          WHEN start =>                     
            scl_ena <= '1';
            busy <= '1';                    
            ack_error <= '0';
            sda_int <= addr_rw(bit_cnt);    
            state <= write_addres;               
          WHEN write_addres =>                 
            IF(bit_cnt = 0) THEN            
              sda_int <= 'Z';              
              bit_cnt <= 7;                
              state <= slv_ack1;           
            ELSE                             
              bit_cnt <= bit_cnt - 1;       
              sda_int <= addr_rw(bit_cnt-1); 
              state <= write_addres;             
            END IF;
          WHEN slv_ack1 =>                
            IF(addr_rw(0) = '0') THEN       
              sda_int <= data_tx(bit_cnt); 
              state <= wr;                  
            ELSE                           
              sda_int <= 'Z';            
              state <= rd;                  
            END IF;
          WHEN wr =>                     
            busy <= '1';               
            IF(bit_cnt = 0) THEN         
              sda_int <= 'Z';               
              busy <= '0';
              bit_cnt <= 7;                 
              state <= slv_ack2;           
            ELSE                             
              bit_cnt <= bit_cnt - 1;       
              sda_int <= data_tx(bit_cnt-1); 
              state <= wr;                  
            END IF;
          WHEN rd =>                         
            busy <= '1';                    
            IF(bit_cnt = 0) THEN            
              sda_int <= '0';             
              bit_cnt <= 7;                
              data_rd <= data_rx;
              busy <= '0';        
              state <= mstr_ack;           
            ELSE                          
              bit_cnt <= bit_cnt - 1;      
              state <= rd;                  
            END IF;
          WHEN slv_ack2 =>                
            IF(ena = '1') THEN                              
              addr_rw <= addr & rw;          
              data_tx <= data_wr;           
              IF(addr_rw = addr & rw) THEN   
                sda_int <= data_wr(bit_cnt); 
                state <= wr;                
              ELSE                           
                state <= start;             
              END IF;
            ELSE                            
              state <= stop;                
            END IF;
          WHEN mstr_ack =>                  
            IF(ena = '1') THEN                              
              addr_rw <= addr & rw;         
              data_tx <= data_wr;            
              IF(addr_rw = addr & rw) THEN   
                sda_int <= 'Z';             
                state <= rd;                
              ELSE                           
                state <= start;              
              END IF;    
            ELSE                             
              state <= stop;                 
            END IF;
          WHEN stop =>                      
            busy <= '0';                    
            state <= ready;                  
        END CASE;
            
      ELSIF(data_clk = '0' AND data_clk_prev = '1') THEN  
        CASE state IS
          WHEN slv_ack1 =>                          
            IF(sda /= '0') THEN                     
              ack_error <= '1';                     
            END IF;
          WHEN rd =>                                
            data_rx(bit_cnt) <= sda;                
          WHEN slv_ack2 =>                          
            IF(sda /= '0') THEN                     
              ack_error <= '1';                     
            END IF;
          WHEN stop =>
            scl_ena <= '0';                         
          WHEN OTHERS =>
            NULL;
        END CASE;
      END IF;
    END IF;
  END PROCESS;  

  WITH state SELECT
    sda <=       data_clk_prev WHEN start,     --generate start bit
                 NOT data_clk_prev WHEN stop,  --generate stop bit
                 sda_int WHEN OTHERS;              
    scl <= 'Z' WHEN scl_ena = '0' ELSE scl_clk;
END logic;
-- cpu.vhd: Simple 7-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2023 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Dominik Horut <xhorut01@stud.fit.vutbr.cz>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (0) / zapis (1)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic;                      -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'

   -- stavove signaly
   READY    : out std_logic;                      -- hodnota 1 znamena, ze byl procesor inicializovan a zacina vykonavat program
   DONE     : out std_logic                       -- hodnota 1 znamena, ze procesor ukoncil vykonavani programu (narazil na instrukci halt)
   
 );
end cpu;



-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

 -- pri tvorbe kodu reflektujte rady ze cviceni INP, zejmena mejte na pameti, ze 
 --   - nelze z vice procesu ovladat stejny signal,
 --   - je vhodne mit jeden proces pro popis jedne hardwarove komponenty, protoze pak
 --      - u synchronnich komponent obsahuje sensitivity list pouze CLK a RESET a 
 --      - u kombinacnich komponent obsahuje sensitivity list vsechny ctene signaly. 

  -- signaly pc
  signal pc_reg : std_logic_vector(12 downto 0);
  signal pc_inc : std_logic;
  signal pc_dec : std_logic;

  -- signaly ptr
  signal ptr_reg : std_logic_vector(12 downto 0);
  signal ptr_inc : std_logic;
  signal ptr_dec : std_logic;

  -- signaly cnt
  signal cnt_reg : std_logic_vector(7 downto 0);
  signal cnt_inc : std_logic;
  signal cnt_dec : std_logic;

  -- signal mx1
  signal mx1_select : std_logic;

  -- signal mx2
  signal mx2_select : std_logic_vector(1 downto 0);

  -- stavy FSM
  type fsm_state is (s_idle, s_fetch, s_decode, s_move_left, s_move_right,
                     s_val_inc, s_val_inc2, s_val_dec, s_val_dec2,
                     s_while_lbr, s_while_rbr, s_seek_lbr, s_seek_rbr, 
                     s_while_lbr2, s_while_rbr2, s_while_break, s_seek_lbr2, s_seek_rbr2,
                     s_print, s_print2, s_read, s_read2, s_read3,
                     s_init, s_init_done, s_return, s_halt, s_reset, s_nop);
  
  -- signaly FSM
  signal pstate : fsm_state := s_idle;
  signal nstate : fsm_state;

  begin

-- PC -- adresa instrukci --
pc: process (RESET, CLK, pc_reg, pc_inc, pc_dec)

  begin

    if(RESET='1') then
    pc_reg <= (others=>'0');
  
    elsif(rising_edge(CLK)) then
      
        if(pc_inc='1') then
          pc_reg <= pc_reg + 1;

        elsif(pc_dec='1') then
          pc_reg <= pc_reg - 1;

        end if;
      
    end if;
end process;

-- PTR -- adresa pameti --
ptr: process (RESET, CLK, ptr_reg, ptr_inc, ptr_dec)

  begin

    if(RESET='1') then
    ptr_reg <= (others=>'0');
  
    elsif(rising_edge(CLK)) then
      
        if(ptr_inc='1') then
          ptr_reg <= ptr_reg + 1;

        elsif(ptr_dec='1') then
          ptr_reg <= ptr_reg - 1;

        end if;
      
    end if;
end process;

-- CNT -- pocitadlo zavorek --
cnt: process (RESET, CLK, cnt_reg, cnt_inc, cnt_dec)

begin 

    if(RESET='1') then
      cnt_reg <= (others=>'0');
    
    elsif(rising_edge(CLK)) then
      
      if(cnt_inc='1') then
        cnt_reg <= cnt_reg + 1;
      
      elsif (cnt_dec='1') then
        cnt_reg <= cnt_reg - 1;
      
      end if;

    end if;

end process;

-- MX1 - ADDR = PC nebo PTR
mx1: process (pc_reg, ptr_reg, mx1_select)

begin

  case mx1_select is

    when '0' =>           
      DATA_ADDR <= pc_reg;
  
    when '1' =>
      DATA_ADDR <= ptr_reg;
    
    when others =>
      
  end case;

end process;

-- MX2 - IN_DATA nebo DATA_RDATA -1 nebo DATA_RDATA +1
mx2: process (IN_DATA, DATA_RDATA, mx2_select)

begin

  case mx2_select is

    when "00" =>
      DATA_WDATA <= IN_DATA;
  
    when "01" =>
      DATA_WDATA <= DATA_RDATA - 1;
    
    when "10" =>
      DATA_WDATA <= DATA_RDATA + 1;
    
    when "11" =>
      DATA_WDATA <= (others => '0');
    
    when others => 
  
  end case;

end process; 

---------------- F S M ----------------

-- Present state
present_state : process (CLK, RESET)

begin

    if(RESET='1') then
      pstate <= s_reset;
    
    elsif(rising_edge(CLK)) then

      if(EN='1') then
      pstate <= nstate;

      end if;
    
    end if;
      
end process;

-- Next state
next_state : process (pstate, IN_VLD, OUT_BUSY, EN, DATA_RDATA)

begin
  
      -- nulovani signalu pri kazde CLK
      pc_inc <= '0';
      pc_dec <= '0';

      ptr_inc <= '0';
      ptr_dec <= '0';

      cnt_inc <= '0';
      cnt_dec <= '0';

      mx1_select <= '0';
      mx2_select <= "00";

      DATA_RDWR <= '0';
      OUT_WE <= '0';
      IN_REQ <= '0'; 

  case pstate is

    when s_reset =>
      -- vse se vynuluje a prechazime zpet na idle
      DATA_RDWR <= '0';
      DATA_EN <= '0';

      READY <= '0';
      DONE <= '0';
      nstate <= s_idle;

    when s_idle =>
        nstate <= s_init; -- zaciname inicializovat
      
    when s_init => 
      mx1_select <= '1';  -- adresa dat
      DATA_EN <= '1';     -- povolujeme cteni
      DATA_RDWR <= '0';   -- cteci rezim

      if (DATA_RDATA /= X"40") then
        ptr_inc <= '1';   -- dokud nenarazime na @ zvysujeme ptr

      else
      nstate <= s_init_done;

      end if;

    when s_init_done =>
      READY <= '1';       -- ptr je nastaven
  
      nstate <= s_fetch;  -- jdeme cist prvni instrukci programu

    when s_fetch => 
      mx1_select <= '0';  -- cteme z pc
      DATA_EN <= '1';     -- povolujeme cteni
      nstate <= s_decode; -- jdeme dekodovat instrukci
    
      
    when s_decode =>

      case DATA_RDATA is
        
        when X"3E" => nstate <= s_move_right; -- >
        when X"3C" => nstate <= s_move_left;  -- <
        when X"2B" => nstate <= s_val_inc;    -- +
        when X"2D" => nstate <= s_val_dec;    -- -
        when X"5B" => nstate <= s_while_lbr;  -- [
        when X"5D" => nstate <= s_while_rbr;  -- ]
        when X"7E" => nstate <= s_while_break;-- ~
        when X"2E" => nstate <= s_print;      -- .
        when X"2C" => nstate <= s_read;       -- ,
        when X"40" => nstate <= s_return;     -- @
        when X"00" => nstate <= s_halt;     
        when others => nstate <= s_nop;
      
      end case;
    
    -- stav pro pripad fallthrough switche
    when s_nop =>
        pc_inc <= '1';
        nstate <= s_fetch;
    
    --- 1. TAKT INCREMENT ---
    when s_val_inc => 
        mx1_select <= '1';  -- cteme z ptr
        DATA_EN <= '1';     -- povolujeme cteni
        DATA_RDWR <= '0';   -- rezim cteni
        pc_inc <= '1'; 
        nstate <= s_val_inc2;

    --- 2. TAKT INCREMENT ---   
    when s_val_inc2 =>
        mx1_select <= '1';  -- cteme z ptr
        mx2_select <= "10"; -- WDATA bude prespano na RDATA + 1 
        DATA_EN <= '1';     -- povolujeme pristup k datum
        DATA_RDWR <= '1';   -- rezim zapisu
        nstate <= s_fetch;

    --- 1. TAKT DECREMENT ---
    when s_val_dec => 
        mx1_select <= '1';  -- cteme z ptr
        DATA_EN <= '1';     -- povolujeme pristup k datum
        DATA_RDWR <= '0';   -- rezim cteni
        pc_inc <= '1';      
        nstate <= s_val_dec2;

    --- 2. TAKT DECREMENT ---  
    when s_val_dec2 =>
        mx1_select <= '1';  -- cteme z ptr
        mx2_select <= "01"; -- WDATA bude prepsano na RDATA - 1
        DATA_EN <= '1';     -- povolujeme pristup k datum
        DATA_RDWR <= '1';   -- rezim zapisu
        nstate <= s_fetch;
        
    --- MOVE LEFT ---
    when s_move_left =>
        ptr_dec <= '1';     -- s ptr jdeme doleva
        pc_inc <= '1';
        nstate <= s_fetch;

    --- MOVE RIGHT ---
    when s_move_right =>
        ptr_inc <= '1';     -- s ptr jdeme doprava
        pc_inc <= '1';
        nstate <= s_fetch;
    
    --- 1. TAKT PRINT ---
    when s_print =>
        mx1_select <= '1';  -- cteme z ptr
        DATA_EN <= '1';     -- povolujeme pristup k datum
        DATA_RDWR <= '0';   -- rezim cteni 
        nstate <= s_print2;
    
    
    --- 2. TAKT PRINT ---
    when s_print2 => 
        mx1_select <= '1';  -- cteme z ptr
        DATA_EN <= '1';     -- povolujeme pristup k datum
        DATA_RDWR <= '0';   -- rezim cteni
        
        if(OUT_BUSY='1') then -- periferie zaneprazdnena
          nstate <= s_print2; -- cekam az nebude zaneprazdnena

        else
          OUT_WE <= '1';          -- povolujeme zapis
          OUT_DATA <= DATA_RDATA; -- vypis na LCD
          pc_inc <= '1'; 
          nstate <= s_fetch;

        end if;
    
    --- 1. TAKT READ ---
    when s_read =>
        IN_REQ <= '1';      -- pozadujeme data

        if(IN_VLD /= '1') then -- cekame na dokonceni prenosu
          nstate <= s_read;

        else
          nstate <= s_read2;   -- pokracujeme k dokonceni cteni

        end if;
    
    --- 2. TAKT READ ---
    when s_read2 =>
        IN_REQ <= '1';      -- pozadujeme data
        mx2_select <= "00"; -- cteme INPUT
        mx1_select <= '1';  -- cteme z ptr
        DATA_EN <= '1';     -- povolujeme pristup k datum
        DATA_RDWR <= '1';   -- rezim zapisu
        nstate <= s_read3;
    
    --- 3. TAKT READ ---
    when s_read3 =>
        pc_inc <= '1';
        nstate <= s_fetch;
    
    --- 1. TAKT WHILE LEVA ZAVORKA ---    
    when s_while_lbr =>
        mx1_select <= '1';  -- cteme z ptr
        pc_inc <= '1';     
        DATA_EN <= '1';     -- povolujeme pristup k datum
        DATA_RDWR <= '0';   -- rezim cteni
        nstate <= s_while_lbr2;
    
    --- 2. TAKT WHILE LEVA ZAVORKA ---
    when s_while_lbr2 =>

        -- na mem[ptr] je 0 -> KONEC WHILE CYKLU
        if (DATA_RDATA = X"00") then  
          cnt_inc <= '1';     -- zaciname pocitat zavorky
          mx1_select <= '0';  -- cteme z pc
          DATA_EN <= '1';     -- povolujeme pristup k datum 
          DATA_RDWR <= '0';   -- rezim cteni
          nstate <= s_seek_rbr; -- jdeme hledat spravnou pravou zavorku 
        
        -- na mem[ptr] neni 0 -> POKRACUJEME VE WHILE CYKLU
        else 
          nstate <= s_fetch;

        end if;

    --- 1. TAKT HLEDANI PRAVE ZAVORKY ---
    when s_seek_rbr =>
        mx1_select <= '0';    -- cteme z pc
        DATA_EN <= '1';       -- povolujeme pristup k datum
        DATA_RDWR <= '0';     -- rezim cteni

        if (DATA_RDATA = X"5B") then    -- nalezena leva zavorka
          cnt_inc <= '1';
          
        elsif (DATA_RDATA = X"5D") then -- nalezena prava zavorka 
          cnt_dec <= '1';

        end if;   
          pc_inc <= '1';
          nstate <= s_seek_rbr2;  
      
    --- 2. TAKT HLEDANI PRAVE ZAVORKY ---
    when s_seek_rbr2 =>

        -- dosli jsme ke spravne prave zavorce
        if (cnt_reg = "00000000") then 
        nstate <= s_fetch;
        
        -- nedosli jsme ke spravne prave zavorce - hledame dal
        else
        nstate <= s_seek_rbr;

        end if;
    
    --- 1. TAKT WHILE PRAVA ZAVORKA ---
    when s_while_rbr => 
        mx1_select <= '1';  -- cteme z ptr
        DATA_EN <= '1';     -- povolujeme pristup k datum
        DATA_RDWR <= '0';   -- rezim cteni 
        nstate <= s_while_rbr2;
    
    --- 2. TAKT WHILE PRAVA ZAVORKA ---
    when s_while_rbr2 =>
        mx1_select <= '1';  -- cteme z ptr
        DATA_EN <= '1';     -- povujeme pristup k datum
        DATA_RDWR <= '0';   -- rezim cteni

        -- na mem[ptr] je 0 -> KONEC WHILE CYKLU
        if (DATA_RDATA = X"00") then 
          pc_inc <= '1';
          nstate <= s_fetch;
        
        -- na mem[ptr] neni 0 -> POKRACUJEME VE WHILE CYKLU
        else
          cnt_inc <= '1';
          pc_dec <= '1';
          nstate <= s_seek_lbr;

        end if; 
    
    --- 1. TAKT HLEDANI LEVE ZAVORKY ---
    when s_seek_lbr => 
        mx1_select <= '0';  -- cteme z pc
        DATA_EN <= '1';     -- povolujeme pristup k datum
        DATA_RDWR <= '0';   -- rezim cteni


        if (DATA_RDATA = X"5B") then    -- nalezena leva zavorka
          cnt_dec <= '1';

        elsif (DATA_RDATA = X"5D") then -- nalezena prava zavorka
          cnt_inc <= '1';
        
        end if;

        nstate <= s_seek_lbr2;
    
    --- 2. TAKT HLEDANI LEVE ZAVORKY ---
    when s_seek_lbr2 =>

        -- nalezli jsme spravou zavorku - pokracujeme ve while cyklu
        if (cnt_reg = "00000000") then 
        pc_inc <= '1';
        nstate <= s_fetch;
    
        -- nenalezli jsme spravnou zavorku - pokracujeme v hledani
        else
        pc_dec <= '1';
        nstate <= s_seek_lbr;
        
        end if;
      
      --- BREAK ---
      when s_while_break =>
        mx1_select <= '0';    -- cteme z pc
        DATA_EN <= '1';       -- povolujeme pristup k datum
        DATA_RDWR <= '0';     -- rezim cteni

        if (DATA_RDATA /= X"5D") then -- hledam pravou zavorku aktualniho while cyklu 
          pc_inc <= '1';
          nstate <= s_while_break;

        else                          -- nasel jsem, posouvam se dal 
        nstate <= s_fetch;

        end if;
    
    --- RETURN -> KONEC PROGRAMU ---
    when s_return =>
        DONE <= '1';      -- vykonavani programu dokonceno
        nstate <= s_halt; -- zaciname haltovat
    
    --- HALT -> ZASTAVENI PROGRAMU ---
    when s_halt =>
        nstate <= s_halt;
          
    when others => 
  
    end case;

  end process;

end behavioral;


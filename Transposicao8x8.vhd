library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity image_process is
end image_process;

architecture Behavioral of image_process is

COMPONENT image1
  PORT (
    clka : IN STD_LOGIC;
    addra : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
END COMPONENT;

COMPONENT image2
  PORT (
    clka : IN STD_LOGIC;
    wea : IN STD_LOGIC_VECTOR(0 DOWNTO 0);
    addra : IN STD_LOGIC_VECTOR(5 DOWNTO 0);
    dina : IN STD_LOGIC_VECTOR(7 DOWNTO 0);
    douta : OUT STD_LOGIC_VECTOR(7 DOWNTO 0)
  );
END COMPONENT;

signal done,clk : std_logic := '0';
signal wr_enable : STD_LOGIC_VECTOR(0 DOWNTO 0) := "0";
signal addr_rom,addr_ram: STD_LOGIC_VECTOR(5 DOWNTO 0) := (others => '0');
signal data_rom,data_in_ram,data_out_ram : STD_LOGIC_VECTOR(7 DOWNTO 0) := (others => '0');
signal row_index,col_index,finalizaT : integer := 0;

begin

--the original image of size 3*4 stored here in rom.
--[22,12,200,126,
--127,128,129,255,
--10,0,1,98]
image_rom : image1 port map(Clk,addr_rom,data_rom);
--the transpose of image1, of size 4*3, is stored here in ram.
--[22,127,10,
--12,128,0,
--200,129,1,
--126,255,98]
image_ram : image2 port map(Clk,wr_enable,addr_ram,data_in_ram,data_out_ram);

--generate the clock.
clk <= not clk after 5 ns;


--transpose the image1 into image2.
--To do this I have to store the pixel at location (a,b) into location (b,a).
process(clk)
begin
    if(falling_edge(clk)) then
        if(done = '0') then
addr_rom <= addr_rom + "0001"; --start reading each pixel from rom
            
            --row and column index of the image.
            if(col_index = 7) then  --check if last column has reached
col_index <= 0; --reset it to zero.
                if(row_index = 7) then --check if last row has reached.
row_index <= 0; --reset it to zero
done <= '1'; --the processing is done.
                else    
row_index <= row_index + 1; --increment row index.
                end if;
            else
col_index <= col_index + 1; --increment column index.
            end if;     
--
      
wr_enable <= "1"; --write enable for the RAM
data_in_ram <= data_rom; --store the current read data from rom into ram.
addr_ram <= conv_std_logic_vector((col_index*8 + row_index),6); --set the address for RAM.
			else
				if (finalizaT < 2) then  
        --this segment reads the transposed image(data written into RAM).
					wr_enable <= "0";  --after processing write enable is disabled
					addr_rom <= "000000"; 
            if(addr_ram = "111111") then 
					addr_ram <= "000000";
					finalizaT <= finalizaT + 1;
            else
					addr_ram <= addr_ram + 1;
            end if; 
        end if; 
    end if;  
end if;	 
end process;    

end Behavioral;

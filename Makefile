NAME=projet_pfa

all:
	make -C src/
	mv src/$(NAME) ./

opt:
	make opt -C src/

clean:
	make clean -C src/

fclean:
	rm $(NAME)
	make clean -C src/

re: fclean all

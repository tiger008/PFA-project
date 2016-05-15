NAME=projet_pfa

all:
	make -C src/
	mv src/$(NAME) ./

opt:
	make opt -C src/
	mv src/$(NAME) ./

clean:
	make clean -C src/

fclean:
	make clean -C src/
	rm -f $(NAME)

re: fclean all

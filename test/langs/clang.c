typedef struct struct_tag {
	int field;
} typedef_tag;

int main(int argc, char **argv[])
{
	const char *s = "a string\n";
	const char *s2 = "a string with a \" in the middle\n";
	const char *s3 = "a string with a \\ and \" in the middle\n";

	func_call(argc);

	return 0;
}
